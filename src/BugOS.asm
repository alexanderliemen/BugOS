;--------------------------------------------------------------------------------
;
;	BugOS v0.3.3b (c) 2000 by Alexander Liemen
;	<alexander@liemen.net>
;
;--------------------------------------------------------------------------------

#include "ti86asm.inc"

_check_basic	equ		$46AB   ; checks for basic program / _check_asm didn't work
_ldhl_8z		equ		$4343	; Sets 8 bytes starting at the address pointed to by HL to zero. (Asmstudio86)
_getcode        equ     $4068	; get scancode

progcounter = _textShadow			; counter of all programs
wnd_counter = _textShadow+1			; counter of programs in the current folder
row			= _textShadow+2			; cursor row
col			= _textShadow+3			; cursor col
window		= _textShadow+4			; window in folder (scroll level)
flag		= _textShadow+5			; just temp flag used for loading the static VAT
vat			= _textShadow+6			; pointer to the static VAT
wnd_vat		= _textShadow+8			; pointer to the dynamic fat of the current folder
cur_folder	= _textShadow+10		; cur_folder (index for folders reloc)
max_wnd		= _textShadow+11		; number of windows
name		= _textShadow+12		; temp var for stuff dealing with a var name (exec, finding stuff...)


.org _asm_exec_ram

	nop
	jp ProgStart
	.dw $0001  
	.dw ShellTitle  
	.dw Icon


ShellTitle:
	.db "BugOS v0.3.3b (c) 2000 by Alexander Liemen",0

;
;  Start of program
;
ProgStart:

	ld hl,progend
	ld (vat),hl

	call init

	ld a,1
	call display_cursor

	call key_loop
	jp exit 


;  
;  keyboard handler
;
key_loop:
	call _getcode
	cp K_DOWN
	jp z,kdown
	cp K_UP
	jp z,kup
	cp K_RIGHT
	jp z,kright
	cp K_LEFT
	jp z,kleft
	cp K_F1
	jp z,kf1
	cp K_F2
	jp z,kf2
	cp K_F3
	jp z,kf3
	cp K_F4
	jp z,kf4
	cp K_F5
	jp z,kf5
	cp K_PLUS
	jp z,contrast_up
	cp K_MINUS
	jp z,contrast_down
	cp K_ENTER
	jp z,kenter
	cp K_SECOND
	jp z,kenter
	cp K_ALPHA
	jp z,power_off
	cp K_STO
	jp z, switch_folder
	cp K_CUSTOM
	jp z,customize
	cp K_EXIT
	jp z,done
	cp K_MORE
	jp z,scroll_wnd
	cp K_DEL
	jp z,delete_prog
	cp K_LEFTPAR
	jp z,hidden_folder
	jr key_loop

;
;  Enter got pressed
;
kenter:
	call exec_prog
	jp ProgStart 

;
;  Up key got pressed
;
kup:
	xor a
	call display_cursor	 ; erase cursor
	
	ld a,(col)   
	cp 2 				 ; test if in right part of window
	jr z, _right_col	 ; right part of window
	ld a,(row)  
	cp 1  				 ; test if on first row
	jr z,prev_wnd  		 ; on first row -> scroll back
	dec a 	 			 ; not on first row
	ld (row),a 			 ; go up
	jr kup_end 			 ; finish
prev_wnd:   			 ; scroll back
	ld a,(window)  
	cp 1  				 ; check if already in first window
	jr z,kup_end  		 ; finish if already in first window
	dec a  				 ; not in first window
	ld (window),a 		 ; set window to previous one
	ld a,6     
	ld (row),a  		 ; set cursor to bottom
	ld a,2
	ld (col),a  		 ; set cursor to right part of window
	call draw_all
	jr kup_end 			 ; finish
_right_col:
	ld a,(row)
	cp 1   			 	 ; check if on top -> go to left part of window
	jr z,prev_col 		 ; to go to left part of window
	dec a  
	ld (row),a 			 ; not on top -> go up
	jr kup_end 		 	 ; finish
prev_col:
	ld (col),a  		 ; go to left part of window
	ld a,6
	ld (row),a  		 ; set cursor to the bottom

kup_end:
	ld a,1
	call display_cursor  ; display cursor
	jp key_loop

;
;  Down key got pressed
;
kdown:

	xor a			 
	push bc
	call display_cursor	 ; erase cursor
	pop bc

	call num_of_progs
	xor a
	cp c			
	jr nz, both_rows	; both sides contain progs
	ld a,(row)
	cp b				; check if already at the bottom
	jr z,kdend			; if yes -> finish
	ld a,(row)			
	inc a				; go one down
	ld (row),a	
	jr kdend			; finish
both_rows:
	ld a,(col)
	cp 2				; check if at left or right part
	jr z,right_col			; right part
	ld a,(row)
	cp 6				; check if at the bottom
	jr z,next_col			; change cursor to right side
	inc a
	ld (row),a			; go one down
	jr kdend			; finish
next_col:
	ld a,1
	ld (row),a			; set cursor to the top
	inc a
	ld (col),a			; set cursor to the right side
	jr kdend			; finish
	
right_col:
	ld a,6
	cp c				; check if scrolling is possible
	jr c,other_windows		; yes, next window holds progs
	ld a,(row)
	cp c				; check if already at the bottom
	jr z,kdend			; finish
	inc a
	ld (row),a			; go down
	jr kdend			; finish
other_windows:
	ld a,(row)			
	cp 6				; check if at the bottom
	jr z,next_window		; -> scroll
	inc a
	ld (row),a			; go one down
	jr kdend			; finish
next_window:
	ld a,1
	ld (row),a			; set cursor to top and left side
	ld (col),a
	ld a,(window)
	inc a				
	ld (window),a		; scroll to next window
	call draw_all

kdend:
	ld a,1
	call display_cursor	; display the cursor
	jp key_loop

;
;  Right key got pressed
;
kright:
	xor a
	call display_cursor
	call num_of_progs
	ld a,(col)		
	cp 1				; check if at the left side
	jr z,go_right			; -> set cursor to right side
	ld a,c
	cp 7				; check next window for progs
	jr c,__set_end			; doesn't have any -> finish
	sub 6
	ld c,a
	ld a,(row)
	ld b,a
	ld a,c
	cp b				; less progs than (row) on next window

	jr c,set_end		; -> handle special case
	ld a,(window)
	inc a			
	ld (window),a		; scroll to next window
	ld a,1
	ld (col),a			; set cursor to left side
	call draw_all
	jr kr_end			; finish
go_right:
	ld a,(row)		
	ld d,a
	ld a,c
	cp d
	jr c, _set_end		; (row) larger than # of progs on right side
	ld a,2			
	ld (col),a			; set cursor to right side
	jr kr_end			; finish

__set_end:
	ld a,c
	ld (row),a			; set cursor to right side
	jr kr_end			; finish

_set_end:
	cp 0				; check if right side contains progs
	jr z, ___set_end	; no -> handle case
	ld a,2
	ld (col),a			; set cursor to right side
	ld a,c
	ld (row),a			; set cursor to bottom of right side
	jr kr_end			; finish

___set_end:
	ld a,b
	ld (row),a			; set cursor to bottom of left side
	jr kr_end

set_end:
	ld a,1
	ld (col),a			; set cursor to left side
	ld a,c
	ld (row),a			; set cursor to position
	ld a,(window)
	inc a	
	ld (window),a		; scroll to next window
	call draw_all

kr_end:
	ld a,1
	call display_cursor	; display cursor
	jp key_loop


;
; Left key got pressed
;
kleft:
	xor a
	call display_cursor	; erase cursor

	ld a,(col)
	cp 2				; right side of window?
	jr z, go_left		; -> set cursor to left side
	ld a,(window)
	cp 1				; first window?
	jr z,set_beg		; -> set cursor to top of window
	dec a
	ld (window),a		; scroll window back
	ld a,2
	ld (col),a			; set cursor to right side
	call draw_all
	jr kl_end			; finish
go_left:
	dec a
	ld (col),a			; set cursor to left side			
	jr kl_end			; finish

set_beg:
	ld a,1
	ld (row),a			; set cursor to top of window

kl_end:
	ld a,1
	call display_cursor	; display cursor
	jp key_loop

;
;  "More" got pressed, inc window or return to window 1
;
scroll_wnd:
	ld a,(max_wnd)
	ld b,a
	ld a,(window)
	cp b
	jr z, first_one
	inc a
	ld (window),a
	jr upd_scrolled_wnd	
first_one:
	ld a,1
	ld (window),a
upd_scrolled_wnd:
	call draw_all
 	ld a,1
	ld (row),a
	ld (col),a
	call display_cursor
	jp key_loop

;
;  clear some values...
;
clear:
	ld a,1
	ld (row),a
	ld (col),a
	ld (window),a
	call draw_all
	ld a,1
	call display_cursor	; display cursor
	ret

draw_all:
	call _clrLCD
	call draw_interface
	call display_progs	; redraw interface with new folder
	call draw_info_wnd

	ret
;
; F1 got pressed
;
kf1:
	xor a
	jr change_it
;
; F2 got presed
;
kf2:
	ld a,1
	jr change_it
;
; F3 got pressed
; 
kf3:
	ld a,2
	jr change_it

;
; F4 got pressed
;
kf4:
	ld a,3

change_it:

	ld (cur_folder),a		; change to folder
	call clear				; update display
	jp key_loop

;
; F5 got pressed - Help folder
;
kf5:
	ld a,4
	ld (cur_folder),a
	call _clrLCD
	call draw_interface

	call display_about	; display credits

	jr help_key_loop	; jump to special keyboard handler

;
;  Adjust contrast up/down
;
contrast_up:
	ld a,1
	jr contrast
contrast_down:
	ld a,-1
contrast:
    ld	 hl, _contrast		; get the current contrast
    add	 a,(hl)				; add either 1 or substract one depending on mode
    and	 $1F				; make sure it's within the boundaries
    ld	 (hl),a				; set the RAM location
    out	 (2),a				; tell the display to update the contrast
	
	jp key_loop


;
; "Sleep" mode
;
power_off:
    ld a,1
    out (3),a
    halt
    ld a,11
    out (3),a        
	jp key_loop

;
; End of standard keyboard handler
;
done:
	ret

;
; Help screen keyboard handler
;
help_key_loop:

	call _getkey
	cp kF1
	jr z,kf1
	cp kF2
	jr z,kf2
	cp kF3
	jr z,kf3
	cp kF4
	jr z,kf4
	cp kF5
	jr z,kf5
	cp kNext
	call z, display_help	
	cp kExit
	jr z,done
	jr help_key_loop

;
; Display help screen
;
display_help:
	call _clrLCD
	call draw_interface
	ld de,$0906
	ld (_penCol),de
	ld hl, arrows
	call _vputs
	ld de,$0F06
	ld (_penCol),de
	ld hl, more
	call _vputs
	ld de,$1506
	ld (_penCol),de
	ld hl, func_keys
	call _vputs
	ld de,$1B06
	ld (_penCol),de
	ld hl, switch
	call _vputs
	ld de,$2106
	ld (_penCol),de
	ld hl, hidden
	call _vputs
	ld de,$2706
	ld (_penCol),de
	ld hl,del
	call _vputs

	ld de,$0943
	ld (_penCol),de
	ld hl,run
	call _vputs
	ld de,$0F43
	ld (_penCol),de
	ld hl, custom
	call _vputs
	ld de,$1543
	ld (_penCol),de
	ld hl, display
	call _vputs
	ld de,$1B43
	ld (_penCol),de
	ld hl,power
	call _vputs
	ld de,$2143
	ld (_penCol),de
	ld hl,exit_str
	call _vputs
	ret	


;
;	displays the trashed programs
;
hidden_folder:
	ld a,5
	ld (cur_folder),a
	call clear
	jp key_loop
	

;
;  if a program is in a normal folder then it will be moved to
;  the trash folder, if it's already in the trash folder then it
;  will be physically deleted
;
delete_prog:
	ld a,(cur_folder)
	cp 5
	jr z,kill_it			; already in trash folder -> physically delete it
	ld a,kF6				; move_it routine will expect a key at this entry point
	jp move_it				; use a part of the switch_folder routine
kill_it:					; physically delete it
	call get_abs_addr
	call _delvar	; delete the var with the input from _findsym
	call load_vat	; reload the static VAT
	call clear		; update display
	jp key_loop


;
;  gets the absolute address stuff of the current program
;
get_abs_addr:
	call get_offset	; get offset in BugOS VAT
	inc hl
	ld a,(hl)		; get length of name
	ex de,hl		; save hl in de
	ld hl,name+1		
	ld (hl),$12	 	; build valid var structure -> 1st byte = $12
	inc hl
	ld (hl),a	 	; second byte = length of name
	inc hl
	ex de,hl		; hl - VAT pointer, de - var struc pointer
	ld b,0
	ld a,(hl)		; get lenght -> bc
	ld c,a
	inc hl
	ldir			; copy name from VAT to var struct
	ld hl,name+1		; 
	rst 20h   		; mov 10 bytes from (hl) to op1
	rst 10h   		; findsym - searches for var in op1
	ret c

	ret

;
; Calc number of progs which are left in the current
; window and the next ones
; b returns left collum of current window
; c returns total number of progs left beginning
; on with the right collum of the current window
;
; used to check if we can move left/right/up/down

num_of_progs:
	ld a,(window)
	ld b,a
	xor a
np_loop:			; calc progs of previous windows
	add a,12
	djnz np_loop
	sub 12
	
	ld b,a
	ld a,(wnd_counter)
	sub b			; sub # from progcounter (total #)
	cp 7			; check if more than 6 progs
	jr nc,np4		; yes -> hande this case
	ld b,a			; no -> set b to result and c to 0
	ld c,0	
	jr npend		; finish
np4:
	sub 6
	ld b,6			; left collum 6 progs
	ld c,a			; c progs left at the right(col/windows)
	
npend:				; finish
	ret


;
;   executes a prog; 
;
exec_prog:
	
	ld hl,vat_ptr		
	rst 20h				
	rst 10h				
	ld (hl),$12

	call get_abs_addr

	call _check_basic  	; check if basic prog in op1
	jr z, exec_asm		; nope then it must be an asm prog
exec_basic:	
    call _clrLCD
    call _homeup		; do some clean up before
    call _runindicon		; running basic prog
    call _exec_basic
	ret
exec_asm:
	call _exec_assembly	; exec asm prog		
	call _ram_page_7
lab:
	nop
	nop
	ret

clear_desc:
	ld hl,$ff11
	ld b,7
clrprog_loop:		; clean up prog description field
	call _ld_hl_11z
	xor a
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl	
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	inc hl
	inc hl
	djnz clrprog_loop
	ret
;
;  displays the cursor
;
display_cursor:
	
	ld c,a			; if 0 -> erase cursor, 1 - display it
	ld a,(wnd_counter)
	cp 0
	jp z,dc_end
	call get_offset		; get offset of prog in BugOS VAT
	inc hl
	push hl			; push it on the stack
	ld a,(row)
	ld b,a
	ld a,4
c1:					; calculate screen y pos
	add a,6
	djnz c1

	ld (_penRow),a
	ld a,(col)
	ld b,a	
	ld a,-32
c2:					; calculate screen x pos
	add a,38
	djnz c2

	ld (_penCol),a
	ld b,(hl)		; save lenght of name in hl
	inc hl
	ld a,c			
	cp 1			; check if erase or display mode
	jr c, not_inverse	; not_inverse = erase
	jr inverse		; = display
not_inverse:
	call _vputsn	; display string not inversed 
	pop hl			; clean up stack 
	ret				; finish
inverse:
	set textInverse, (iy+textflags)  ; set system flags to invert mode
	call _vputsn			 ; display name inverted
	res textInverse, (iy+textflags)	 ; reset system flag

	call clear_desc
	xor a
	ld b,5
	ld de,15
	ld hl, $fe1d
clrsize_loop:		; clean up size field
	ld (hl),a
	inc hl
	ld (hl),a
	add hl,de
	djnz clrsize_loop

	ld hl, $fd4d
	xor a
	ld b,8
	ld de,15

clricon_loop:		; clean up icon field
	ld (hl),a
	inc hl
	ld (hl),a
	add hl,de
	djnz clricon_loop

	pop hl			; get pointer to VAT which we save before

	ld a,(hl)		; save length of name
	ex de,hl		; save hl in de
	ld hl,name+1		
	ld (hl),$12		; TI var struc - 1st byte = type of prog
	inc hl
	ld (hl),a		; 2nd byte = name length
	inc hl
	ex de,hl		; hl = VAT pointer, de- name struc pointer
	ld b,0
	ld a,(hl)		; ld bc with length of name
	ld c,a
	inc hl
	ldir			; copy name from VAT to var struc
	ld hl,name+1
	rst 20h			; mov 10 bytes from (hl) to op1

	ld de,$2065
	ld (_penCol),de

	rst 10h			; get absolute address in bde	

	push de			; save de on stack
	call 	_get_size_word	; get word at bde == program size
	ex		de,hl 	; 
	xor		a	
	call _disp_hl	; display value in hl

	

	pop de			; get saved de -> restore bde pointer
check:
	call _check_basic	; check if basic prog
	jp z, display_asm	; asm? -> display prog description
	
	ld hl,name+2		
	ld a,(hl)		; save length of name in a
	ld b,a			
	inc hl
	ld a,6
	ld (_penCol),a
	ld a, 49
	ld (_penRow),a
	call _vputsn	; display length byte string (name)

	ld de,$0964
	ld (_penCol),de
	ld hl,basic_str	; display program type "B_86"
	call _vputs		
dc_end:
	jp ex_end		; finish
	

;
; moves a program to another folder
;
switch_folder:
	ld a,(wnd_counter)	
	cp 0
	jp z,sw_done			; check if there's a prog, no? well nothing to move then
	call clear_desc			; clear desc field
	ld a,06
	ld (_penCol),a
	ld a, 49
	ld (_penRow),a
	ld hl,move				
	call _vputs				; display "move prog to 1/2/3....." string in desc field
switch_loop:
	call _getkey			
	cp kF1
	jr z,move_it
	cp kF2
	jr z,move_it
	cp kF3
	jr z,move_it
	cp kF4
	jr z,move_it
	cp kExit
	jr nz, switch_loop		; no valid key pressed -> check again
	ld a,1					; exit got pressed
	call display_cursor
	jp key_loop
move_it:
	push af

	call get_abs_addr

	call _ram_page_7
	dec hl
	dec hl
	dec hl
	dec hl
	pop af
	sub $0c2		; sub kF2 to get a value between 0-3 (folder 1-4)
	ld (hl),a
	call _ram_page_7
	call load_vat		; reload VAT to view changes
	
	ld a,(col)
	cp 2
	jr nz, check_left
	ld a,(row)
	cp 1
	jr z,left
	dec a
	ld (row),a
	jr cont
left:
	ld (col),a
	ld a,6
	ld (row),a
	jr cont
check_left:
	ld a,(row)
	cp 1
	jr z, scroll
	dec a
	ld (row),a
	jr cont
scroll:
	ld a,(window)	
	cp 1
	jr nz,scroll_back
	xor a
	ld (wnd_counter),a
	jr cont
scroll_back:
	dec a
	ld (window),a
	ld a,6
	ld (row),a
	ld a,2
	ld (col),a	
cont:
	call draw_all
	ld a,1
	call display_cursor	; display cursor
sw_done:
	jp key_loop


;
;  Customize a folder
;
customize:
	call _clrLCD
	call draw_interface

	ld a,06
	ld (_penCol),a
	ld a, 49
	ld (_penRow),a
	ld hl, cust
	call _vputs
	ld b,6

	ld de,$0202
	ld (_curRow),de				; set cursor to $0202
	set 4,(iy+indicflags)		; set "alpha" flag
	res 5,(iy+indicflags)		; reset "lwralpha" flag
	call _getkey
	add a,$19					; add $19 to get real ASCII val
	ld c,a	
	call _putc					; display it
	ld a,c
	ld hl,name
	ld (hl),a					; save first char
	inc hl
	ld c,$1f
	dec b
get_str:
	set 4,(iy+indicflags)
	set 5,(iy+indicflags)		; set lwr_alpha flag
	push hl
	call _getkey			
	pop hl
	cp kExit
	jr z, go_on
	cp kEnter
	jr z,save
	add a,c						; add $1f to get real ASCII val
	ld (hl),a					; save char 
	push hl
	call _putc					; display it
	pop hl
	inc hl
	djnz get_str
save:
	push bc
	ld hl,folders
	ld a,(cur_folder)
	add a,a
	ld d,0	
	ld e,a
	add hl,de
	call _ldhlind				; get offset of folder string 
	ld de,name
	ex de,hl
	pop bc
	push de
debug:
	ld a,6
	sub b
	ld b,0
	ld c,a

	push af
	ldir						; write new name over old name
	ex de,hl
	pop af
	ld c,a
	cp 6
	jr z,zero					; already max length -> zero terminate string
zero_it:
	ld (hl),$20					; not max length -> insert a white space
	inc hl
zero:
	ld (hl),0					; zero terminate string
save_it:
	ld hl, vat_ptr
	rst 20h
	rst 10h						; get absolute address of BugOS
	call _ex_ahl_bde			; exchange bde<->ahl
	call _load_ram_ahl			; load ram page + hl = offset in ram page
	pop de
	push de
	push hl
	ex de,hl
	ld de, _asm_exec_ram
	xor a
	sbc hl,de					; calc offset of folder corresponding to _asm_exec_ram
	pop de
	add hl,de					; add offset to offset in ram page
	ld d,0
	ld e,4
	add hl,de					; add 4 (skip size and whatever)
	pop de
	ex de,hl
	ld b,0
	ld c,6
	ldir						; write new name over old one in the actual binary
	call _ram_page_7			; swap ram page 7 back in

go_on:
	call draw_all				; update display
	ld a,1
	call display_cursor	; display cursor
	res 6, (iy+indicflags)
	jp key_loop

;
; display description, size and icon of asm prog
;
display_asm:

	push bc
	push de	
	ld de,$0964
	ld (_penCol),de
	ld hl,asm_str	; display prog type "A_86"
	call _vputs

	ld de,$1965
	ld (_penCol),de

	pop de
	pop bc

	call _ex_ahl_bde
	call _load_ram_ahl	; set ram page, save offset in hl
	ld de,4
	add hl,de
	xor a
	cp (hl)			; check 4th byte
	jr nz, nodesc		; if 0 then there's a program description
	push hl			; save hl on stack
	add hl,de		; skip next 4 bytes
	ld a,1			
	cp (hl)			; if byte is one -> icon exists
	jr nz, noicon

	add hl,de		; skip next 4 bytes
	ld e,(hl)		; next 2 bytes describe
	inc hl			; the offset of Icon in prog
	ld d,(hl)		; in respect to _asm_exec_ram

	ld hl,_asm_exec_ram	; start of prog
	ex de,hl		; save in de
	xor a
	sbc hl,de		; sub offset from exec_start
	pop de			; restore saved hl in de
	push de			; save de on stack twice
	push de
	add hl,de		; add offset to Icon from start
	inc hl			; add 2 bytes and we got the right
	inc hl			; offset
	ld b,105		; set sprite coordinates
	ld c, 20
	push hl			; save hl on stack
	pop ix 			; restore it in ix (sprite source)
	call PutSprite		; display the damn thing

	pop hl			; restore one of the saved des in hl
	ld de,4
	add hl,de		; skip next 4 bytes
	
noicon:
	inc hl			; skip next 2 bytes
	inc hl

	ld e,(hl)
	inc hl			; get offset of program description
	ld d,(hl)		; in respect to _asm_exec_ram

	ld hl,_asm_exec_ram
	ex de,hl
	sbc hl,de		; sub offset from _asm_exec_ram
	pop de			; restore 2nd saved de
	add hl,de		; add offset of desc to start
	ld a,6
	ld (_penCol),a
	ld a, 49
	ld (_penRow),a
	call _vputs		; display the program description

	jr ex_end		; finish
nodesc:	

	ld hl,name+2		; ld name from VAT
	ld a,(hl)		; save size in a
	ld b,a			; a -> b since vputsn wants it in b
	inc hl
	ld a,6
	ld (_penCol),a
	ld a, 49
	ld (_penRow),a
	call _vputsn		; display simple prog name
	ld hl, nodesc_str	
	call _vputs		; display "No Description..."

ex_end:				; finish
	
	ret
;
;   displays VAT with respect to index (scroll level)
;
display_progs:
;	ld hl,progend-(10*12)	; hl -> start of vat
	call load_wnd_vat		; build dynamic VAT for the cur folder


	ld a,(wnd_counter)		
	cp 0					; check if there's a program to be displayed
	jr nz, display_them		; display the progs
	ld hl, no_progs_str		; display "no progs found"
	ld de,$1814
	ld (_penCol),de
	call _vputs
	jp dpend				; finish
display_them:

	set textInverse, (iy+textflags)	
	ld de,$0169
	ld (_penCol),de

	ld hl,window_number+1
	ld a,(window)
	add a,48					; display the window number in the top bar
	ld (hl),a
	ld a,(wnd_counter)
	dec a
	ld l,a
	ld h,0
	ld a,12
	call _divHLbyA
	ld a,l
	inc a
	ld (max_wnd),a
	add a,48
	ld hl,window_number+3
	ld (hl),a
	ld b,5
	ld hl,window_number
	call _vputsn
	res textInverse, (iy+textflags)

	ld hl,(wnd_vat)			
	ld de,10*12				; 10 bytes per item, 12 items per window (2*6)
	xor a
	sbc hl,de
	ld a,(window)
	ld b,a
	ld de,10*(6*2)
dp1:				; skip all the previous windows
	add hl,de
	djnz dp1

	call num_of_progs	

	push bc			; save result on stack
	ld c,6
dp7:
	ld a,10
	ld (_penRow),a
	ld a,c			; set pen to upper left corner
	ld (_penCol),a
	ld d,0
left_col:
	push bc			; 
	inc hl
	ld a,(hl)		; get length of name
	ld b,a			; a -> b cuz of vputsn
	ld e,a			; save in e 
	inc hl

	call _vputsn		; display prog name
	ld a,(_penRow)
	add a,6			; restore x pos of pen
	ld (_penRow),a
	ld a,c			; add 6 to y pos of pen
	ld (_penCol),a
	xor a			; clear carry
	sbc hl,de		; restore hl (start of prog entry)
	ld e,8
	add hl,de		; get next VAT entry (one inc + 8 = 9)
	pop bc			; clean up stack
	djnz left_col
	pop bc			; get saved bc - num_of_progs result 

	xor a			
	cp c			; check if progs on the right side
	jr z,dpend		; nope? -> finish
	ld a,6			
	cp c			; check if less than 6
	jr nc,dp8		; yes? -> loop c times 
	ld c,6			; no? -> loop 6 times
dp8:
	ld b,c			; loop c times 
	ld c,0			; set # of progs on the right side to 0
	push bc			; save bc on stack for loop 
	ld c,44			; set x pen pos to right part of window
					; this time the code will think it deals
					; with the left part although it actually
					; display the right part of the window
					; c is set to 0 -> will finish after this 
					; run
	jr dp7			; loop 

dpend:				; finish
	ret

;
; calculates the offset of the program which the cursor points to
;
get_offset:
;	progend+(window*9*12)+col*9*6+row*9
;	ld hl,progend-(10*12)-(10*6)-9
	ld hl,(wnd_vat)
	ld de, (10*12)+(10*6)+10
	xor a
	sbc hl,de
	ld a,(window)
	ld b,a
	ld de,10*(6*2)	; (2 cols * 6 entries) * 9 bytes = 1 wnd
l3:					; add windows
	add hl,de
	djnz l3
	
	ld a,(col)
	ld b,a
	ld de,10*6		; 6 progs per col * 9bytes per entry
l4:					; add cols
	add hl,de
	djnz l4

	ld a,(row)
	ld b,a
	ld de,10			; 1 row = 1 entry = 9 bytes
l5:					; add rows
	add hl,de
	djnz l5
	ret				; return with offset in hl

;
;	searches the BugOS static VAT and creates the dynamic VAT 
;	out of it which contains only the programs to be displayed
;	in the folder - same format as the static ones -> all routines
;	will work on either VAT, (vat) contains the current VAT which 
;	is to be used
;
load_wnd_vat:
	xor a
	ld (wnd_counter),a		; reset the wnd_counter
	ld hl,progend
	ld a,(progcounter)
	ld b,a					; loop (progcounter) times
	ld d,0
	ld e,10					; 10 bytes per item
wnd_vat_loop:
	push bc
	ld a,(cur_folder)
	cp 0					; check if "standard" folder -> unknown progs will be inserted here
	jr z,std_folder
	ld b,a					; another folder -> check if prog belongs to folder
	ld a,(hl)
	cp b					; check if cur folder == prog folder
	jr nz, next				; nope, it doesn't
	call insert_in_vat		; voila, insert it
	jr next
std_folder:					; check if program already belongs 
	ld a,(hl)				; to another folder
	cp 1					; yup
	jr z, next				
	cp 2					; yup
	jr z, next				
	cp 3					; yup
	jr z, next
	cp 5
	jr z, next
	call insert_in_vat		; nope -> display it in the "standard" folder
next:
	add hl,de				; advance to next item
	pop bc
	djnz wnd_vat_loop
	ret

insert_in_vat:
	push hl
	ld hl,(wnd_vat)
	ld de,10
	xor a
	sbc hl,de
	ld a,(wnd_counter)	
	inc a			      ; inc progcounter for folder
    ld (wnd_counter), a
	ld b,a
	
	ld d,0
	ld e,10
insert_loop:	
	add hl, de		; go to end of BugOS VAT
	djnz insert_loop	
	
	ex de,hl		; save pointer in de
	pop hl
	ld b,0			; 10 bytes per prog
	ld c,10
	ldir			; insert prog in BugOS VAT
	ld d,0
	ld e,10
	xor a
	sbc hl,de
	ret
;
;   searches the VAT for programs and stores them at progend+(n-1)*9 n==number of prog
;   name stored as length byte string
;   1st byte = length of name; 2nd to 9th byte = name
;
load_vat:
	ld hl,vat_ptr		
	rst 20h				
	rst 10h				
	ld (hl),$0C		

	xor a
	ld (progcounter),a
	call _ram_page_7; swap VAT from RAM page 7 to $8000-$BFFF
	ld hl, $BFFF	; beginning of the VAT
_vat_loop:
	ld de,($D298)   ; end of VAT  (it is stored backwards beginning with the "end" at $BFFF)
	inc de			 
	or a			; clear flags

	sbc hl,de		; set carry flag if we're at the end (-> inc de above
	jr c, _end		
	
	add hl,de		; restore hl

	ld a,(hl)
	and %00011111   ; get the bits we need (object type byte)
	ld (flag),a
	dec hl			; least significant abs addr byte
	dec hl			; middle byte
	dec hl          ; most signicicant abs add byte
	dec hl			; unused byte - shell folder byte
	ld a,(hl)
	ld (name),a
	dec hl			; length byte for name
	ld b,(hl)		; get length of name
	ld a,b
	ld (name+1),a		; first byte = length
	dec hl       
	ld ix, name+2	; skip length byte
_load_name:			; copy name to var name
	ld a,(hl)
	ld (ix),a     	; save name
	inc ix			; dec source, inc dest because the VAT and all the information
					; is stored backwards -> Testprog would be in the VAT as gorptseT
	dec hl			
	djnz _load_name	

	ld a,(name+2)	; skip length byte
	cp '#'          ; there are two progs named # and ! that are not visible
	jr z, _vat_loop	; by anything else, don't know what they are there for
	cp '!'			;  -> ignoring them just like other shells do
	jr z, _vat_loop

	xor a		   
	ld (ix),a	   	; zero terminate string??? hhmm wtf?

	ld a,(flag)		; check if var is a program
	cp $12			; check if program
	jr nz, _vat_loop

	push hl

	ld hl,(vat)
	ld de,10
	xor a
	sbc hl,de
	ld a,(progcounter)	
	inc a			; inc progcounter
    ld (progcounter), a
	ld b,a
	
	ld d,0
	ld e,10
_prog_loop:	
	add hl, de		; go to end of BugOS VAT
	djnz _prog_loop	
	
	ex de,hl		; save pointer in de
	ld hl, name		
	ld b,0			; 10 bytes per prog
	ld c,10
	ldir			; insert prog in BugOS VAT

	pop hl
	jr _vat_loop	; loop
_end:
	call _ram_page_7; swap Ram page back

	ld hl, (vat); beginning of BugOS VAT
    ld a,(progcounter)
	ld b,a
	ld d,0
	ld e,10
_prog_loop2:	
	add hl, de		; go to end of BugOS VAT
	djnz _prog_loop2
	ld (wnd_vat),hl

	ld hl,vat_ptr		
	rst 20h				
	rst 10h				
	ld (hl),$12

	ret


;
;  Draws the interface
;

draw_interface:  ; draw the shell interface

	ld hl,$fc00
	ld b,8
di_loop:
	inc hl
	push bc
	ld b,14
_di_loop:	
	ld (hl),255
	inc hl
	djnz _di_loop
	pop bc
	inc hl
	djnz di_loop

	ld ix,spr1
	ld b,0
	ld c,0
	call PutSprite
	ld ix,spr2
	ld b,120
	ld c,0
	call PutSprite
	ld ix, spr3
	ld b,0
	ld c,56
	call PutSprite
	ld b,120
	ld c,56
	call PutSprite

	ld b,6
	ld d,0
	ld ix,spr6
left_frame:
	push bc
	ld a,d
	add a,8
	ld c,a
	ld d,a
	ld b,0
	push de
	push ix
	call PutSprite
	pop ix
	pop de
	pop bc
	djnz left_frame

	ld hl,$ff80
	ld b,8
di2_loop:
	inc hl
	push bc
	ld b,14
_di2_loop:	
	ld (hl),255
	inc hl
	djnz _di2_loop
	pop bc
	inc hl
	djnz di2_loop

	ld b,6
	ld d,0
	ld ix,spr7

right_frame:
	push bc
	ld a,d
	add a,8
	ld c,a
	ld d,a
	ld b,120
	push de
	push ix
	call PutSprite
	pop ix
	pop de
	pop bc
	djnz right_frame


	ld a,6
	ld (_penCol),a
	ld a,1
	ld (_penRow),a
	set textInverse, (iy+textflags)	
	ld hl, shell
	call _vputs

	ld a,29
	ld (_penCol),a
	ld a,1
	ld (_penRow),a
	ld hl,slash	
	call _vputs

	ld a,36
	ld (_penCol),a
	ld a,1
	ld (_penRow),a
	ld hl, folders
	ld a,(cur_folder)
	
	add a,a
	ld d,0	
	ld e,a
	add hl,de
	call _ldhlind
	call _vputs
		
	ld b,5
	ld c,6
	ld d,0	
	ld e,0
folder_loop:
	ld a,(cur_folder)
	ld d,a
	ld a,e
	srl a
	cp d
	jr nz,not_current
	res textInverse, (iy+textflags)

not_current:
	ld d,0
	ld a,c
	ld (_penCol),a
	ld a,57
	ld (_penRow),a
	ld hl,folders
	add hl,de
 	call _ldhlind
	call _vputs
	ld a,c
	add a,27
	ld c,a
	inc e
	inc e
	set textInverse, (iy+textflags)
	djnz folder_loop

	res textInverse, (iy+textflags)

	ld hl, $ff00
	ld c,16
	call h_line	


	ret

display_about:
	ld de,$0402
	ld (_curRow),de
	ld hl, logo
	call _puts

	ld de,$0503
	ld (_curRow),de
	ld hl, copyright
	call _puts

	ld de,$0304
	ld (_curRow),de
	ld hl, coder
	call _puts

	ld a,6
	ld (_penCol),a
	ld a, 49
	ld (_penRow),a
	ld hl, email
	call _vputs

	ld a,60
	ld (_penCol),a
	ld a, 41
	ld (_penRow),a
	ld hl, help_str
	call _vputs

	ret

draw_info_wnd:
	ld c,40
	ld d, %00000001
	ld hl, $fc89		; right line
	call v_line

	ld de,$0951
	ld (_penCol),de
	ld hl, type_str
	call _vputs

	ld de,$1452
	ld (_penCol),de
	ld hl, icon_str
	call _vputs

	ld de,$2052
	ld (_penCol),de
	ld hl, size_str
	call _vputs

	ld de,$2952
	ld (_penCol),de
	ld hl, free_str
	call _vputs

	call _memchk	; _get_free_mem didn't work
	ld de,$2965
	ld (_penCol),de
	call _disp_hl	; routine from Joshua S.

	ret

PutSprite:
 ld h,63			;shifted to $fc with add hl,hl
 ld a,c
 add a,a			;a*4
 add a,a
 ld l,a
 add hl,hl			;hl*4 (what was c has been mlt by 16
 add hl,hl
 ld a,b				;a/8
 rra
 rra
 rra
 or l				;add to more significant f
 ld l,a
 ld a,7				;use bottom 7 bits for counter
 and b
 ld d,a				;save counter copy in d
 ld e,8				;8 rows
 push bc

ps_loop:
 ld b,d				;get saved bit offset in b
 ld a,(ix)			;get this byte of the sprite in a
 inc ix				;point ix to the next byte of the sprite
 ld c,0
 call bit_shift
 or (hl)			;change this to or if you want
 ld (hl),a
 inc l
 ld a,(hl)
 or c				;also changable to or (if you changed the first)
 ld (hl),a
 ld a,15			;add 15 to hl (now ready for next row)
 call add_hl_a
 dec e				;counter (8 rows)
 jr nz,ps_loop
 pop bc
 ret

add_hl_a:			;add hl,a
 add a,l
 ld l,a
 ret nc
 inc h
 ret

bit_shift:			;while(b=<0)
 dec b				; a>>c
 ret m
 srl a
 rr c
 jr bit_shift

;
;  help functions
;
h_line:    ; horizontal line starting at (hl)
	ld b,c
	or 255
_h_line:
	ld (hl),a
	inc hl
	djnz _h_line
	ret

v_line:   ; vertical line starting at (hl)
	ld b,c
	ld c,d
	ld de,16	
_v_line:
	ld a,(hl)
	or c
	ld (hl),a
	add hl,de
	djnz _v_line
	ret

; original routine from Joshua S.
; modified it to fix some output bugs

_disp_hl:
	push af
	push hl
	ld hl,bytes_str
	call _ldhl_8z
	pop hl
	pop af
	ld		de,-1
	ld		(_curRow),de
	call	_dispAHL
	dec		hl
	dec 	hl

	ld de, bytes_str
dhl_loop:
	inc hl
	ld a,(hl)
	cp 0
	jr z, dhl_end
	cp $20
	jr z,dhl_loop
	ex de,hl
	ld (hl),a
	inc hl
	ex de,hl
	jr dhl_loop
	
dhl_end:
	ld hl,bytes_str
	call	_vputs
	ret

init:  ; clrscr, deactivate bussy indi...
	call _runindicoff
	xor a
	ld (progcounter),a	
	ld (cur_folder),a
	call load_vat
	call clear

	ret

exit:  ; clear all the shit, reset stuff...

	call _clrScrn        ; clean up
	call _runindicon
    res 1,(iy+5)
    res 5,(iy)
    res onInterrupt,(iy+onflags)
	jp _homeup
;
;  variables...
;

Icon:
.db 8,1
.db %11111111
.db %11000011
.db %10100101
.db %10011001
.db %10011001
.db %10100101
.db %11000011
.db %11111111


free_str:
.db "Free:",0

type_str:
.db "Type:",0

asm_str:
.db "A_86",0

basic_str:
.db "B_86",0

icon_str:
.db "Icon:",0

size_str:
.db "Size:",0

bytes_str:
.db "        ",0

nodesc_str:
.db "  -  No Description!",0

folders:
.dw main, games, math, misc, about, trash

shell:
.db "BugOS",0

vat_ptr:
.db $12,$5,"BugOS   ",0

cust:
.db "Enter new name: ",0

main:
.db "Main  ",0

games:
.db "Games ",0

math:
.db "Math  ",0

misc:
.db "Misc  ",0

trash:
.db "Trash ",0

about:
.db "?",0

logo:
.db "BugOS v0.3.3b",0

copyright:
.db "(c) 2000 by",0

coder:
.db "Alexander Liemen",0

email:
.db "Email: alexander@liemen.net",0

help_str:
.db "Press More for Help",0

slash:
.db "-",0

arrows:
.db "Arrows - Move",0

run:
.db "2nd/Enter - Run",0

func_keys:
.db "F(1-5) - Folders",0

display:
.db "+/- - Contrast",0

power:
.db "Alpha - Power off",0

no_progs_str:
.db "No Programs!",0

switch:
.db "Sto-> - Move",0

custom:
.db "Cstm - Rename",0

move:
.db "Move to folder 1/2/3/4: ",0

del:
.db "Del - Trash it",0

more:
.db "More - Next wnd",0

hidden:
.db "H - Trash",0

exit_str:
.db "Exit - Exit",0

window_number:
.db "(1/1)",0


spr1:
.db %00001111	
.db %00111111	
.db %01111111	
.db %01111111	
.db %11111111	
.db %11111111	
.db %11111111	
.db %11111111	

spr2:
.db %11110000	
.db %11111100	
.db %11111110	
.db %11111110	
.db %11111111	
.db %11111111	
.db %11111111	
.db %11111111	

spr3:
.db %11111111
.db %11111111
.db %11111111
.db %11111111
.db %01111111
.db %01111111
.db %00111111
.db %00001111

spr4:
.db %11111111
.db %11111111
.db %11111111
.db %11111111
.db %11111110
.db %11111110
.db %11111100
.db %11110000

spr6:
.db %11100000	
.db %11100000	
.db %11100000	
.db %11100000	
.db %11100000	
.db %11100000	
.db %11100000	
.db %11100000	

spr7:
.db %00000111	
.db %00000111	
.db %00000111	
.db %00000111	
.db %00000111	
.db %00000111
.db %00000111
.db %00000111	

noicon_spr:
.db %11111111
.db %10000011
.db %10000101
.db %10001001
.db %10010001
.db %10100001
.db %11000001
.db %11111111


progend:
.end
