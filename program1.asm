;autor: Patryk Czuchnowski

data1 segment

    text_0 db "zero$"
    text_1 db "jeden$"
    text_2 db "dwa$"
    text_3 db "trzy$"
    text_4 db "cztery$"
    text_5 db "piec$"
    text_6 db "szesc$"
    text_7 db "siedem$"
    text_8 db "osiem$"
    text_9 db "dziewiec$"
	
	text_10 db "dziesiec$"
	text_11 db "jedenascie$"
	text_12 db "dwanascie$"
	text_13 db "trzynascie$"
	text_14 db "czternascie$"
	text_15 db "pietnascie$"
	text_16 db "szesnascie$"
	text_17 db "siedemnascie$"
	text_18 db "osiemnascie$"
	text_19 db "dziewietnascie$"
	
	text_20 db "dwadziescia$"
	text_30 db "trzydziesci$"
	text_40 db "czterdziesci$"
	text_50 db "piecdziesiat$"
	text_60 db "szescdziesiat$"
	text_70 db "siedemdziesiat$"
	text_80 db "osiemdziesiat$"

    text_plus db "plus$"
    text_minus db "minus$"
    text_times db "razy$"

	word1_address dw ?
	word2_address dw ?
	word3_address dw ?

	space db " $"
    newline db 10, "$"
    error_text db "Blad danych wejsciowych$"
	start_text db "Wprowadz slowny opis dzialania: $"
	result_text db "Wynikiem jest: $"

    ;bufor do wczytania
    input_buffer_size db 40  ; limit 40 znakow
    input_length db 0   ; liczba odczytanych znakow
    input_buffer db 40 dup(' ') ; bufor wlasciwy

data1 ends


code1 segment

start1:
    ;inicjalizacja stosu
    mov ax, seg stack1
    mov ss, ax
    mov sp, offset stack1start

    ;wrzucenie segmentu danych do ds
	mov ax, seg data1
	mov ds, ax

	;wypisanie tekstu poczatkowego
	mov dx, offset start_text
	call print

    ;wczytanie tekstu
    mov dx, offset input_buffer_size
    call read

    ;wypisanie nowej linii
    mov dx, offset newline
    call print

	;zapisanie adresu 1 slowa
    mov ds:[word1_address], offset input_buffer
	
    ;znalezienie adresu 1 spacji i zamienienie jej na $
	mov dx, ds:[word1_address]
    call space_index
    mov byte ptr ds:[bx], '$'
    add bx, 1

	;zapisanie adresu 2 slowa
	mov ds:[word2_address], bx
	
    ;znalezienie adresu 2 spacji i zamienienie jej na $
	mov dx, ds:[word2_address]
    call space_index
    mov byte ptr ds:[bx], '$'
    add bx, 1
	
	;zapisanie adresu 3 slowa
	mov ds:[word3_address], bx

	;interpretacja 1 liczby
	mov dx, ds:[word1_address]
	call interpret_number
	mov al, bl
	
	;interpretacja 2 liczby
	mov dx, ds:[word3_address]
	call interpret_number
	
	;interpretacja operacji
	mov dx, ds:[word2_address]
	call interpret_operation
	
	;wypisanie tekstu 'wynik to...'
	mov dx, offset result_text
	call print
	
	;wypisanie ewentualnego minusa
	call minus
	
	;podzielenie wyniku z reszta przez 10
	;w al otrzymujemy liczbe dziesiatek, a w ah liczbe jednosci
	mov bl, 10
	xor ah, ah
	xor bh, bh
	div bl
	
	;jesli liczba dziesiatek to 0 to wypisujemy tylko liczbe jednosci
	cmp al,0
	je zero_to_nine
	
	;jesli liczba dziesiatek to 1 to wypisujemy jedno slowo specjalne (np dwanascie, a nie dziesiec dwa)
	cmp al,1
	je ten_to_nineteen
	
	;wypisujemy liczbe dziesiatek
	mov cx, offset text_18
	call print_number
	
	;konczymy program jesli liczba jednostek to zero (zeby bylo np dwadziescia, a nie dwadziescia zero)
	cmp ah,0
	je program_end
	
	;wypisujemy spacje
	mov dx, offset space
	call print

	;wypisujemy liczbe jednosci
	mov al, ah
	mov cx, offset text_0
	call print_number

    ;koniec programu
	program_end:
    mov ax, 4C00h
    int 21h
	
zero_to_nine:
	mov al, ah
	mov cx, offset text_0 ;wypisujemy liczbe jednosci
	call print_number
	jmp program_end
	
ten_to_nineteen:
	mov al, ah	
	mov cx, offset text_10 ;wypisujemy liczbe jednosci ale zaczynamy od tekstu 10 - idac do przodu otrzymamy tekst z przedzialu 10-19
	call print_number
	jmp program_end
	
minus: ;wypisuje minus jesli liczba w al jest ujemna i neguje al, zeby byla dodatnia
	cmp al, 0
	jns minus_end ;jesli liczba jest dodatnia to wracamy
	mov dx, offset text_minus
	call print
	mov dx, offset space
	call print
	neg al
	ret

minus_end:
	ret
	
print_number: ;input - liczba w al, startowy adres stringa w cx, procedura wypisuje liczbe slowami
	push ax
	push cx
	
print_number_loop: ;przechodzimy o tyle stringow do przodu ile wynosci liczba w al
	dec al
	cmp al, -1
	je print_number_end
	
	push bx
	call string_len ;do bx dostajemy dlugosc stringa
	add cx, bx ;przeskakujemy do kolejnego napisu
	pop bx
	
	inc cx
	
	jmp print_number_loop
	
	
print_number_end: ;wypisujemy string na ktorym skonczylismy
	push dx
	mov dx, cx
	call print
	pop dx
	pop cx
	pop ax
	ret

interpret_operation: ;input - 1 liczba w al, 2 liczba w bl, adres stringa w dx, output - wynik dzialania w al
	push ax
	push cx
	
	;sprawdzamy czy dzialaniem jest dodawanie
	mov cx, offset text_plus
	call compare_string
	cmp al, 1
	je interpret_operation_plus
	
	;sprawdzamy czy dzialaniem jest odejmowanie
	mov cx, offset text_minus
	call compare_string
	cmp al, 1
	je interpret_operation_minus
	
	;sprawdzamy czy dzialaniem jest mnozenie
	mov cx, offset text_times
	call compare_string
	cmp al, 1
	je interpret_operation_times
	
	pop cx
	pop ax
	
	;jesli zadne z tych dzialan to blad
	jmp error
	
	ret

interpret_operation_plus: ;gdy dzialanie zinterpretowano jako dodawanie
	pop cx
	pop ax
	add al, bl
	ret
	
interpret_operation_minus: ;gdy dzialanie zinterpretowano jako odejmowanie
	pop cx
	pop ax
	sub al, bl
	ret
	
interpret_operation_times: ;gdy dzialanie zinterpretowano jako mnozenie
	pop cx
	pop ax
	mul bl
	ret

	
interpret_number: ;input - adres poczatkowego stringa w dx, output - liczba w bl
	push ax
	push cx
	mov cx, offset text_0
	mov bl,0
	
interpret_number_loop: 
	;liczymy ilosc przejsc petli i przeskakujemy do kolejnych stringow
	;jesli ktorys z nich bedzie rowny to konczymy, ilosc przejsc petli to nasza liczba
	call compare_string
	cmp ax,1
	je interpret_number_end
	
	push bx
	call string_len
	add cx, bx
	pop bx
	
	add cx, 1
	
	add bl, 1	
	cmp bl,10
	jne interpret_number_loop
	;jesli petla przeszla 10 razy (po cyfrach 0-9) i nie znaleziono stringa rownego stringowi w dx to blad
	je error
	
interpret_number_end:
	pop cx
	pop ax
	ret
	
	
string_len: ;input - adres stringa w cx, output - dlugosc stringa w bx
	push ax
    mov bx, cx

string_len_loop: ;petla idzie do przodu po stringu, konczy sie gdy napotka znak $
	mov al, byte ptr ds:[bx]
	cmp al, '$'
    je string_len_end
	add bx,1
	jmp string_len_loop

string_len_end:
	sub bx, cx ;odejmujemy adres poczatkowy stringa od adresu $ w celu otrzymania dlugosci stringa
	pop ax
	ret	
	

compare_string: ;input - adres 1 stringa w cx, a 2 stringa w dx, output - 0 lub 1 w ax, w zaleznosci czy stringi sa takie same
	push bx
	push dx
    mov bp, cx
	mov bx, dx

compare_string_loop: ;petla przechodzi po obu napisach naraz
    mov al, byte ptr ds:[bp]
	mov dl, byte ptr ds:[bx]
	
    cmp al, dl ;jesli znak w jednym stringu jest inny od znaku na tym samym miejscu w drugim stringu to zwracamy 0
    jne compare_string_end_false

    cmp al, '$' ;stringi sie zakoncza i nie napotkalismy roznic to zwracamy 1 
	;(wiemy ze oba stringi sie zakoncza w tym miejscu poniewaz wczesniej sprawdzamy czy znaki na tym miejscu sa rowne, czyli w obu stringach musi tu byc $)
    je compare_string_end_true
    
    inc bp
	inc bx

    jmp compare_string_loop

compare_string_end_true:
	mov ax,1
	pop dx
	pop bx
    ret
	
compare_string_end_false:
	mov ax,0
	pop dx
	pop bx
    ret

space_index: ;input - adres napisu w ds:dx, output - index najblizszej spacji w bx
	push cx
    mov bx, dx

space_index_loop: ;podobnie jak string_len, petla idzie do przodu po stringu, konczy sie gdy napotka znak spacji
    mov cl, byte ptr ds:[bx]
    cmp cl, ' '
    je space_index_end

    cmp cl, '$'
    je space_index_end_not_found ;jesli dojdziemy do konca stringa i nie znalezlismy spacji skaczemy do procedury error
    
    add bx,1

    jmp space_index_loop

space_index_end:
	pop cx
    ret

space_index_end_not_found:
	pop bx
	pop cx
	jmp error
    ret

print: ;input - adres napisu w ds:dx
	push ax
	mov ah, 9 
	int 21h
	pop ax
	ret

read: ;input - adres bufora w ds:dx
	push ax
	push bx
	
    mov	ah,0ah
    int 21h

    ;zamiana entera na nowa linie
    mov bp, dx
    add bp, 1
    mov bl, byte ptr ds:[bp]
    add bl,1
    xor bh,bh
    add bp, bx
    mov byte ptr ds:[bp], '$'
	
	pop bx
	pop ax
    ret

error: ;wypisuje tekst bledu i konczy program z kodem bledu 1
    mov dx, offset error_text
    call print
    mov ax, 4C01h
    int 21h

code1 ends

stack1 segment stack

    dw 300 dup(?)
    stack1start dw ?

stack1 ends

end start1