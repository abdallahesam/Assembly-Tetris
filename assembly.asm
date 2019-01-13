.MODEL FLAT, C    ;Use the flat memory model. Use C calling conventions
option casemap:none

includelib libcmt.lib
includelib libvcruntime.lib
includelib libucrt.lib
includelib libcpmt.lib
includelib legacy_stdio_definitions.lib

extern system:NEAR
extern getch:NEAR ;In order for getch to work just include an *empty* c file in your project 
extern putchar:NEAR

.STACK;Define a stack segment of 1KB 
.DATA    ;Create a near data segment.
;Coulmns and Rows can't be greater than short.MaxValue
Rows = 7
Columns = 15
InitialShapeX = Columns / 2
InitialShapeY = 0
VerticalShape = 1
HorizontalShape = 2
RowLength = Columns + 2
PlayGroundLength = Rows * RowLength
PlayGround db Rows DUP(Columns DUP(" "), 0dh, 0ah), 0
GameEndMessage db "Game Over!", 0dh, 0ah,"Press any key to exit", 0
ClearScreenCommand db "cls",0
CurrentShapeNumber dd HorizontalShape ;My shape is ***
;ShapeX and ShapeY are defiend as integers but there values can't be more than short.MaxValue because I store them in 16-bit registeres
ShapeX dd 0
ShapeY dd 0
DidTouch dd 0
Key dd 0
IsGameOver dd 0
.CODE             ;Indicates the start of a code segment.

;checked
;sets esi to point to the first cell in the shape
MoveEsiToTop PROC
	PUSH eax
	PUSH ebx
	mov esi, offset PlayGround
	mov eax, [ShapeY] 
	mov bx, RowLength
	mul bx ;I didn't move dx because I dont want it and the value is under 16-bits
	add esi, eax
	;Now move the offset to point to the shape first char
	add esi, [ShapeX]
	POP ebx
	POP eax
	ret
MoveEsiToTop ENDP

;checked
;;Erases the shape from its current position
EraseShape PROC
	PUSHAD
	;Calculate the shape top row offset and store it in esi
	call MoveEsiToTop	
	mov ecx, 3

	mov eax, [CurrentShapeNumber]
	cmp eax, VerticalShape
	jne hrz
	;VerticalShape
	;Now clear the shape by replacing the stars with spaces
clrv:
	mov BYTE PTR [esi],' ';clear the cell
	add esi,RowLength ;Advance esi to the next cell in the shape
	LOOP clrv
	jmp exit
hrz:
	;HorizontalShape
clrh:
	mov BYTE PTR [esi],' ';clear the cell
	inc esi ;Advance esi to the next cell in the shape
	LOOP clrh
exit:
	POPAD
	ret 	
EraseShape ENDP

;checked
DrawShape PROC
	PUSHAD
	call MoveEsiToTop	
	mov ecx, 3

	mov edx, [CurrentShapeNumber]
	cmp edx, VerticalShape
	jne hrz
	;VerticalShape
	;Now draw the shape
drwv:
	mov BYTE PTR [esi], '*';draw the cell
	add esi, RowLength ;Advance esi to the next cell in the shape
	LOOP drwv
	jmp exit
hrz:
	;HorizontalShape
drwh:
	mov BYTE PTR [esi], '*';draw the cell
	inc esi ;Advance esi to the next cell in the shape
	LOOP drwh
exit:
	POPAD
	ret 
DrawShape ENDP

;checked
;Checks if the shape has touched another star underneath it or reached the bottom of the playground and sets the value of variable DidTouch accordingly
CheckIfTouched PROC
	PUSHAD
	call MoveEsiToTop	
	mov edx, CurrentShapeNumber
	cmp edx, VerticalShape
	jne hrz
	;VerticalShape
	mov eax, Rows
	sub eax, 3
	cmp eax, [ShapeY]
	je Touch

	mov eax, 3 ;3 is the number of cells of the shape
	mov bx, RowLength
	mul bx
	add esi, eax ;moved esi to point the cell under the shape
	mov al, BYTE PTR [esi]
	cmp al, '*'
	je Touch
	jne NoTouch

hrz:
	mov eax, Rows
	dec eax
	cmp eax, [ShapeY]
	je Touch

	add esi, RowLength ;moved esi to point the cell under the shape
	mov ecx, 3
chk:
	mov al, [esi]
	cmp al, '*'
	je Touch
	inc esi ;advnce to the next cell under the shape
	loop chk
	;In case we finished the loop without jumping to Touch label then its.......NoTouch!
	jmp NoTouch

Touch:
	mov [DidTouch], 1
	jmp exit
NoTouch:
	mov [DidTouch], 0
exit:
	POPAD
	ret 
CheckIfTouched ENDP

;Checks if shape can be moved down and moves it
;checked
MoveShapeDown PROC
	PUSHAD
	call MoveEsiToTop
	cmp [CurrentShapeNumber], VerticalShape
	jne hrz
	;VerticalShape
	mov eax, RowLength
	mov bx, 3
	mul bx ;now eax is the difference between esi and the cell under the shape, dx is useless
	add esi, eax
	cmp BYTE PTR [esi], '*'
	je exit
	jmp MoveDown

hrz:
	;HorizontalShape
	add esi, RowLength ;Now esi points to the first cell under the shape
	mov ecx, 3
CheckCell:
	cmp BYTE PTR [esi], '*'
	je exit
	inc esi
	loop CheckCell
MoveDown:
	call EraseShape	
	inc [ShapeY]
	call DrawShape
exit:
	POPAD
	ret
MoveShapeDown ENDP

;rotation happens around the center
;Checks if the object can be rotated and rotate it
RotateShape PROC
	PUSHAD
	mov eax, [CurrentShapeNumber]
	cmp eax, VerticalShape
	jne hrz
	;VerticalShape
	mov eax, [ShapeX]
	cmp eax, 0 ;in case x is zero then we can't rotate it to be horizntal
	je exit
	cmp eax, Columns ;in case x is at the end of the playground the we can't rotate it to be horizntal
	je exit

	;now lets check if there is a space for it to be rotated
	call MoveEsiToTop	
	add esi, RowLength ;move esi to point to the second cell in the vertical shape
	dec esi ;now esi points to the cell that is to the left of the shape
	cmp BYTE PTR [esi], '*'
	je exit ;There is no space to make it horizntal
	
	add esi, 2;now esi points to the cell that is to the right of the shape
	cmp BYTE PTR [esi], '*'
	je exit ;There is no space to make it horizntal
	;Ok if we reached this line then I *think* it can be rotated
	CALL EraseShape
	inc [ShapeY]
	dec [ShapeX]
	mov [CurrentShapeNumber], HorizontalShape
	call DrawShape
	jmp exit

hrz:	
	;HorizontalShape
	mov eax, [ShapeY]
	cmp eax, 0 ;in case y is zero then we can't rotate it to be vertical
	je exit
	cmp eax, Rows ;in case y is at the end of the playground the we can't rotate it to be vertical
	je exit

	;now lets check if there is a space for it to be rotated
	call MoveEsiToTop	
	inc esi ;move esi to point to the second cell in the Horizontal shape
	sub esi, [RowLength] ;now esi points to the cell that is above the shape
	cmp BYTE PTR [esi], '*'
	je exit ;There is no space to make it horizntal
	
	add esi, RowLength ;move esi to point to the second cell in the Horizontal shape
	add esi, RowLength ;now esi points to the cell that is under the shape
	cmp BYTE PTR [esi], '*'
	je exit ;There is no space to make it horizntal
	;Ok if we reached this line then I *think* it can be rotated
	CALL EraseShape
	dec [ShapeY]
	inc [ShapeX]
	mov [CurrentShapeNumber], VerticalShape
	call DrawShape
exit:
	POPAD
	ret 
RotateShape ENDP

;checked
;Checks if the shape can be moved to the right and move it
MoveShapeToRight PROC
	PUSHAD
	call MoveEsiToTop	
	mov eax, [CurrentShapeNumber]
	cmp eax, VerticalShape
	jne hrz
	;VerticalShape
	mov eax, [ShapeX]
	;remove me
	inc eax
	cmp eax, Columns
	je exit ;can't be moved because it will get out of bounds

	inc esi ;make esi point to the cell to the right of the object
	mov ecx, 3
chkv:
	cmp BYTE PTR [esi], '*'
	je exit ;if there is a star to its right then we can't move it
	add esi, RowLength ;advance esi to the next row
	loop chkv
	jmp mvright
hrz:
	mov eax, [ShapeX]
	add eax, 3
	cmp eax, Columns
	je exit ;can't be moved because it will get out of bounds

	add esi, 3 ;make esi point to the cell to the right of the object
	cmp BYTE PTR [esi], '*'
	je exit

mvright:
	call EraseShape	
	inc [ShapeX]
	call DrawShape	
exit:
	POPAD
	ret
MoveShapeToRight ENDP

;checked
;Checks if the shape can be moved to the left and move it
MoveShapeToLeft PROC
	PUSHAD
	call MoveEsiToTop	
	mov eax, [CurrentShapeNumber]
	cmp eax, VerticalShape
	jne hrz
	;VerticalShape
	mov eax, [ShapeX]
	cmp eax, 0
	je exit ;can't be moved because it will get out of bounds

	dec esi ;make esi point to the cell to the left of the object
	mov ecx, 3
chkv:
	cmp BYTE PTR [esi], '*'
	je exit ;if there is a star to its right then we can't move it
	add esi, RowLength ;advance esi to the next row
	loop chkv
	jmp mvleft
hrz:
	mov eax, [ShapeX]
	cmp eax, 0
	je exit ;can't be moved because it will get out of bounds

	dec esi ;make esi point to the cell to the left of the object
	cmp BYTE PTR [esi], '*'
	je exit

mvleft:
	call EraseShape	
	dec [ShapeX]
	call DrawShape	
exit:
	POPAD
	ret
MoveShapeToLeft ENDP

;checked
;Erases the line with index pointed by eax and shifts the lines above it
EraseLine PROC
	PUSHAD
	mov ecx, eax ;ecx = index of the line to erase
	mov esi, offset PlayGround
	mov bx, RowLength
	mul bx ;eax = offset of the first cell in the row
	add esi, eax ;esi = pointer to the first cell in the row
	add esi, Columns ;esi = pointer to the last cell in the row
	dec esi
	mov edi, esi
	sub edi, RowLength ;edi = pointer to the cell above the last cell in the row
	;mov ecx, edx ;so we loop only (1 - Index]
copyLineAbove:
	push ecx
	mov ecx, Columns
copyCharAbove:
	mov al, [edi]
	mov BYTE PTR [esi], al
	dec esi
	dec edi
	loop copyCharAbove
	;Now esi & edi points to 0ah
	sub esi, 2
	sub edi, 2
	;Now esi & edi points to last elements in there rows
	pop ecx
	loop copyLineAbove

	;Now all the lines are shifted down but PlayGround[0] == PlayGround[1]
	;so lets clear the first line
	mov esi, offset PlayGround
	mov ecx, Columns
clearCell:
	mov BYTE PTR [esi],' '
	inc esi
	loop clearCell
exit:
	POPAD
	ret
EraseLine ENDP

;checked
EraseFullLines PROC
	PUSHAD
	mov ecx, Rows
nextLine:
	mov esi, offset PlayGround
	mov eax, RowLength
	mov bx, cx ;bx = index +1 of the current line
	dec bx
	mul bx ;eax = index of the first cell in the current row
	add esi, eax; esi= pointer to the first cell in the 

	push ecx
	mov ecx, Columns
chkLine:
	cmp BYTE PTR [esi], '*'
	jne continueNextLine
	inc esi
	loop chkLine
	;If we reached this line then its a full line
	pop ecx
	mov eax, ecx
	dec eax ;eax = INDEX of the current line
	call EraseLine	
	inc ecx ;just visualise two CONSIQUTIVE FULL lines and u will know why we should increment ecx
	loop nextLine ;we loop and don't let it fall down because we already poped ecx
continueNextLine:
	pop ecx
	loop nextLine

exit:
	POPAD	
	ret
EraseFullLines ENDP

;checked
PrintPlayGround PROC
	PUSHAD
	push offset ClearScreenCommand
	call system
	add esp, 4 ;Clear the satck after the push
	mov esi, offset PlayGround
	;Frame drop ahead!!
	mov ecx, PlayGroundLength
PrintChar:
	movzx ebx, BYTE PTR [esi]
	push ecx ;Push ecx because the putchar will change its value
	push ebx
	call putchar ;will change eax value 
	add esp, 4 ;Clear the satck after the push ebx
	pop ecx
	inc esi
	loop PrintChar
	POPAD
	ret
PrintPlayGround ENDP

;Reads a char from user and returns it
;it only returns (w,a,s,d)
;in case the user entered an upper-case char it will return the lower-case one
;If the entered char wasn't (w,a,s,d) it will return s
ReadKey PROC
	PUSHAD
	call getch ;the read key will be in eax
	cmp eax, 'w'
	je exit
	cmp eax, 'a'
	je exit
	cmp eax, 's'
	je exit
	cmp eax, 'd'
	je exit

	cmp eax, 'W'
	je CapitalChar
	cmp eax, 'A'
	je CapitalChar
	cmp eax, 'S'
	je CapitalChar
	cmp eax, 'D'
	je CapitalChar
	
	;In case we reached this line then the input wasn't (a,b,c,d)
	mov eax, 's'
	jmp exit

CapitalChar:
	or eax, 100000b	;Set the 5th bit 
exit:
	mov [Key], eax
	POPAD
	ret
ReadKey ENDP

;checks if any of the columns is filled and sets value of IsGameOver variable accordingly
CheckPlayGround PROC
	PUSHAD
	mov esi, offset PlayGround
	mov ecx, Columns
CheckNextColumn:
	cmp BYTE PTR [esi], '*'
	je Over
	inc esi
	loop CheckNextColumn
NotOver:
	mov [IsGameOver], 0
	jmp exit
Over:
	mov [IsGameOver], 1
exit:
	POPAD
	ret
CheckPlayGround ENDP

EndGame PROC
	PUSHAD
	push offset ClearScreenCommand
	call system
	add esp, 4 ;Clear the satck after the push
	mov esi, offset GameEndMessage
PrintChar:
	cmp BYTE PTR [esi], 0
	je RKey
	movzx eax, BYTE PTR [esi]
	push eax
	call putchar ;will change eax value 
	add esp, 4 ;Clear the satck after pushing eax
	inc esi
	jmp PrintChar
RKey:
	call ReadKey
exit:
	POPAD
	ret
EndGame ENDP
main PROC C
	mov [CurrentShapeNumber], HorizontalShape
	mov [ShapeX] ,InitialShapeX
	mov [ShapeY] ,InitialShapeY
	call DrawShape	
	call PrintPlayGround	
lp:
	call ReadKey
	cmp [Key], 's'
	jne NotS
	call MoveShapeDown
	jmp MoveDown
NotS:
	cmp [Key], 'w'
	jne NotW
	call RotateShape
	jmp MoveDown
NotW:
	cmp [Key], 'a'
	jne NotA
	call MoveShapeToLeft
	jmp MoveDown
NotA:
	;The key is d
	call MoveShapeToRight
MoveDown:
	call CheckIfTouched
	cmp [DidTouch], 1
	je Touched
DidntTouch:
	call MoveShapeDown
	call CheckIfTouched
	cmp [DidTouch], 1
	je Touched
	jmp Draw
Touched:
	call DrawShape
	call PrintPlayGround	
	call EraseFullLines
	call CheckPlayGround
	cmp [IsGameOver], 1
	je GameOver
	mov [CurrentShapeNumber], HorizontalShape
	mov [ShapeX], InitialShapeX
	mov [ShapeY], InitialShapeY
Draw:
	call DrawShape	
	call PrintPlayGround	
	jmp lp

GameOver:
	call EndGame
	mov eax, 0 ;I guess this is how you return a value from a C function using stdcall convention
	ret 
main ENDP 
END