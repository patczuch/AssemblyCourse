;autor: Patryk Czuchnowski

data1 segment
    error_text db "Blad danych wejsciowych$"
	colors db 15, 14, 13, 12, 11, 10, 9, 8, 7, 6
	
data1 ends

code1 segment

start1:
    ;inicjalizacja stosu
    mov ax, seg stack1
    mov ss, ax
    mov sp, offset stack1start
	
	xor bx, bx
    mov bl, byte ptr ds:[80h] ;miejsce w pamieci w ktorym zapisana jest dlugosc argumentow
	cmp bx, 2 ;jesli dlugosc argumentow < 2 to blad
	jl error
	
	add bx, 81h ;argumenty zaczynaja sie w 81h, wiec 81h + dlugosc = adres konca arg
	mov byte ptr ds:[bx], '$' ;wrzucamy $ na koniec stringa argumentow (zamiast entera)
	
	mov dx, 81h ;poczatek stringa z argumentami
	call whitespace_index
	mov dx, bp ;przeskakujemy znaki biale na poczatku argumentow
	call whitespace_index
	mov byte ptr ds:[bx], '$' ;zamieniamy pierwszy znak bialy na $ 
	
	call string_to_number ;zamieniamy stringa na liczbe
	cmp ax, 200 ;jesli wiekszy niz 200 to blad
	jg error
	
	mov cl, 2
	div cl ;dzielimy liczbe w ax przez 2 - wynik w al, reszta w ah
	
	mov byte ptr cs:[a], al ;zapisujemy wartosc a

	mov dx, bp ;adres drugiego argumentu (wczesniej obliczony adres ostatniej spacji)
	
	call string_to_number ;zamieniamy stringa na liczbe
	cmp ax, 200 ;jesli wiekszy niz 200 to blad
	jg error
	
	div cl ;ponownie dzielimy przez 2 - wynik w al, reszta w ah
	
	mov byte ptr cs:[b], al ;zapisujemy wartosc b
	
	;wrzucenie segmentu danych do ds
	mov ax, seg data1
	mov ds, ax
	
	;zmiana na tryb graficzny
	mov al,13h
	mov ah,0
	int 10h
	
	;narysowanie poczatkowej elipsy
	mov byte ptr cs:[color], 15
	call draw_ellipse
	
	main_loop:
	;oczekiwanie na dowolny klawisz, dostajemy scancode w ah
	xor ax,ax
	int 16h
	
	cmp ah,4bh ;lewa strzalka
	je left_arrow
	
	cmp ah,4dh ;prawa strzalka
	je right_arrow
	
	cmp ah,50h ;strzalka w dol
	je down_arrow
	
	cmp ah,48h ;strzalka w gore
	je up_arrow
	
	cmp ah,1 ;escape
	je program_end
	
	;procedury do zmiany koloru
	call color_change_array
	call color_change_by_one 
	
	jmp main_loop
	
	program_end:
	;zmiana na tryb tekstowy
	mov al,3h
	mov ah,0
	int 10h
	
	;koniec programu
	mov ax, 4C00h
    int 21h
	
color_change_array:
	push ax
	push bx
	cmp ah,2h ;1 ;jesli klawisz < 1 lub > 0 to nic nie robimy
	jl color_change_array_end
	cmp ah,0bh ;0
	jg color_change_array_end
	
	mov bx, offset ds:[colors] ;obliczamy offset nowego koloru
	mov al, ah
	xor ah, ah
	add bx, ax
	sub bx, 2h
	
	mov al, byte ptr ds:[bx] ; ustawiamy nowy kolor z tablicy
	mov byte ptr cs:[color], al
	
	xor ax,ax
	xor bx,bx
	call ellipse_change
	
color_change_array_end:
	pop bx
	pop ax
	ret
	
color_change_by_one:
	push ax
	push bx
	cmp ah,0ch ;- 
	je color_change_by_one_minus
	cmp ah,0dh ;+
	je color_change_by_one_plus
	;jesli klawisz != - i != + to nic nie robimy

color_change_by_one_end:
	pop bx
	pop ax
	ret

color_change_by_one_plus:
	mov al, byte ptr cs:[color]
	inc al
	mov byte ptr cs:[color], al
	xor ax,ax
	xor bx,bx
	call ellipse_change
	pop bx
	pop ax
	ret
	
color_change_by_one_minus:
	mov al, byte ptr cs:[color]
	dec al
	mov byte ptr cs:[color], al
	xor ax,ax
	xor bx,bx
	call ellipse_change
	pop bx
	pop ax
	ret

left_arrow: ; a-=1
	push ax
	push bx
	mov ax, -1
	xor bx, bx
	call ellipse_change
	pop bx
	pop ax
	jmp main_loop
	
right_arrow: ; a+=1
	push ax
	push bx
	mov ax, 1
	xor bx, bx
	call ellipse_change
	pop bx
	pop ax
	jmp main_loop
	
down_arrow: ; b-=1
	push ax
	push bx
	xor ax, ax
	mov bx, -1
	call ellipse_change
	pop bx
	pop ax
	jmp main_loop
	
up_arrow: ; b+=1
	push ax
	push bx
	xor ax, ax
	mov bx, 1
	call ellipse_change
	pop bx
	pop ax
	jmp main_loop
	
string_to_number: ;input - adres napisu w ds:dx, output - liczba w ax
	push bx
	push cx
	push bp
	push dx
	xor ax, ax
	xor cx, cx
	mov bp, dx
	mov bx, 10
	
string_to_number_loop:
	xor cx, cx
	mov cl, byte ptr ds:[bp] ;zapisujemy obecny znak do cl
	cmp cl, '$' ;jesli dojdziemy do konca napisu to koniec
	je string_to_number_end
	
	cmp cl, '0' ;jesli napotkamy znak mniejszy od '0' to blad
	jl error
	
	cmp cl, '9' ;jesli napotkamy znak wiekszy od '9' to blad
	jg error
	
	mul bx ;mnozymy obecna liczbe przez 10
	
	sub cl, '0' ;odejmujemy '0' od cl w celu zamiany znaku na cyfre
	add ax, cx ;dodajemy cyfre do liczby
	
	inc bp ;przechodzimy do nastepnego znaku
	
	;liczymy na ktorym znaku jestesmy
	mov cx, bp
	pop dx
	sub cx, dx 
	push dx
	cmp cx, 4 ;jesli liczba dluzsza niz 4 znaki to blad 
	;(zeby nie mozna bylo wpisac liczby np 80 000, co nie zmiesci sie na 16 bitach)
	jg error
	
	jmp string_to_number_loop

string_to_number_end:
	pop dx	
	pop bp
	pop cx
	pop bx
	ret
	
whitespace_index: ;input - adres napisu w ds:dx, output - index najblizszego znaku bialego (spacja, tab, newline) w bx 
					;oraz index najdalszej znaku bialego w bp
	push cx
    mov bx, dx

whitespace_index_loop1: ;petla idzie do przodu po stringu, konczy sie gdy napotka znak bialy
    mov cl, byte ptr ds:[bx]
    cmp cl, ' ' ;jesli napotkamy spacje to koniec
    je whitespace_index_end1
	
	cmp cl, 10 ;jesli napotkamy tab to koniec
    je whitespace_index_end1
	
	cmp cl, 9 ;jesli napotkamy newline to koniec
    je whitespace_index_end1

    cmp cl, '$' ;jesli dojdziemy do konca napisu i nie znalezlismy znaku bialego skaczemy do procedury error
    je error 
    
    inc bx ;idziemy do nastepnego znaku

    jmp whitespace_index_loop1

whitespace_index_end1:
	push bx
	dec bx
	
	whitespace_index_loop2: ;petla szuka ostatniego znaku bialego
		inc bx ;idziemy do nastepnego znaku
		
		;jesli napotkamy znak niebedacy spacja, tabem lub newline to koniec
		mov cl, byte ptr ds:[bx]
		cmp cl, ' ' 
		je whitespace_index_loop2
			
		mov cl, byte ptr ds:[bx]
		cmp cl, 9 
		je whitespace_index_loop2
		
		mov cl, byte ptr ds:[bx]
		cmp cl, 10
		je whitespace_index_loop2
	
	mov bp, bx
	
	pop bx
	pop cx
    ret
	
ellipse_change: ;ax - zmiana a, bx - zmiana b
	push cx
	
	xor cx,cx
	mov cl, byte ptr cs:[color]
	push cx
	;wyczyszczenie ekranu (narysowanie elipsy o poprzednim rozmiarze ale czarnej)
	mov byte ptr cs:[color], 0
	call draw_ellipse
	
	;policzenie nowego a (dodanie ax do a)
	mov cx, word ptr cs:[a]
	add cx, ax
	mov word ptr cs:[a], cx
	
	;policzenie nowego b (dodanie bx do b)
	mov cx, word ptr cs:[b]
	add cx, bx
	mov word ptr cs:[b], cx
	
	;sprawdzenie czy a <= 100 oraz a >= 0
	mov ax, word ptr cs:[a]
	mov bx, 100
	call min
	mov bx, 0
	call max
	mov word ptr cs:[a], ax
	
	;sprawdzenie czy b <= 100 oraz b >= 0
	mov ax, word ptr cs:[b]
	mov bx, 100
	call min
	mov bx, 0
	call max
	mov word ptr cs:[b], ax
	
	;narysowanie nowej elipsy
	pop cx
	mov byte ptr cs:[color], cl
	call draw_ellipse
	
	pop cx
	ret
	
	
min: ;input ax i bx, zwraca min w ax
	cmp ax, bx
	jg min_bx
	ret

min_bx:
	mov ax, bx
	ret
	
max: ;input ax i bx, zwraca max w ax
	cmp ax, bx
	jl max_bx
	ret

max_bx:
	mov ax, bx
	ret

	
sqrt: ;oblicza pierwiastek, input - ax, output - ax
	push bx
	push cx
	mov bx, ax
	xor ax, ax ;poczatkowa liczba = 0
	
sqrt_loop:
	push ax
	
	mov cx, ax
	mul cx ; podnosimy obliczona liczbe do kwadratu
	
	cmp ax, bx ;konczymy gdy obliczona liczba podniesiona do kwadratu jest wieksza od wejsciowej
	jge sqrt_end 
	
	pop ax
	inc ax ; zwiekszamy obliczona liczbe
	
	jmp sqrt_loop
	
sqrt_end:
	pop ax
	pop cx
	pop bx
	ret
	
;-----------------------------------------------
	x dw ?
	y dw ?
	color db ?
;-----------------------------------------------
draw_point: ; zapala punkt na ekranie, koordynaty punktu w x i y, kolor w color
	push ax
	push bx
	
	;ustawienie odpowiedniego adresu segmentowego
	mov ax, 0a000h
	mov es,ax
	
	;policzenie offsetu punktu
	mov ax, word ptr cs:[y]
	mov bx, 320
	mul bx ; ax = ax * bx
	mov bx, word ptr cs:[x]
	add bx, ax
	
	;ustawienie koloru w tym punkcie
	mov al, byte ptr cs:[color]
	mov byte ptr es:[bx], al
	
	pop bx
	pop ax
	ret

draw_4_points_mirrored: ;zapala 4 punkty na ekranie, jeden z nich o koordynatach x i y, reszta odbita lustrzanie wzgledem osi
	push ax
	
	;rysujemy punkt w 1 cwiartce
	push word ptr cs:[x]
	push word ptr cs:[y]
	
	dec word ptr cs:[x]
	dec word ptr cs:[y]

	call draw_point
	
	push word ptr cs:[x]
	
	;rysujemy punkt w 4 cwiartce
	mov ax, 319
	sub ax, word ptr cs:[x]
	mov word ptr cs:[x], ax
	
	call draw_point
	
	;rysujemy punkt w 3 cwiartce
	mov ax, 199
	sub ax, word ptr cs:[y]
	mov word ptr cs:[y], ax
	
	call draw_point
	
	;rysujemy punkt w 2 cwiartce
	pop word ptr cs:[x]
	
	call draw_point
	
	pop word ptr cs:[y]
	pop word ptr cs:[x]
	pop ax
	ret
	
;-----------------------------------------------
	a dw ?
	b dw ?
;-----------------------------------------------
draw_ellipse: ; rysuje elipse, parametry a i b oraz color
	push ax
	push bx
	push cx
	
	cmp word ptr cs:[a], 1 ;jesli a < 1 to nic nie rysujemy
	jl draw_ellipse_end
	
	cmp word ptr cs:[b], 1 ;jesli b < 1 to nic nie rysujemy
	jl draw_ellipse_end

	;wykonujemy te petle a razy
	mov cx, word ptr cs:[a]
	draw_ellipse_loop1:
		push cx
		
		;obliczamy obecne x
		mov ax, word ptr cs:[a] 
		sub ax, cx
		add ax, 160
		
		mov word ptr cs:[x], ax
		sub ax, 160
		
		;sprawdzamy z ktorego ze wzorow skorzystac
		mov cx, word ptr cs:[a]
		mov bx, word ptr cs:[b]
		cmp cx, bx
		jl a_less_than_b

		call get_y2 ;obliczamy y ze wzoru 2
		add ax, 100
		mov word ptr cs:[y], ax
		
		jmp a_less_than_b_end
		a_less_than_b:
		
		call get_y1 ;obliczamy y ze wzoru 1
		add ax, 100 
		mov word ptr cs:[y], ax
		
		a_less_than_b_end:
		
		call draw_4_points_mirrored ; rysujemy punkty

		pop cx
	loop draw_ellipse_loop1
	
	;wykonujemy te petle b razy
	mov cx, word ptr cs:[b]
	draw_ellipse_loop2:
		push cx
		
		;obliczamy obecne y
		mov ax, word ptr cs:[b]
		sub ax, cx
		add ax, 100
		
		mov word ptr cs:[y], ax
		sub ax, 100
		
		;sprawdzamy z ktorego ze wzorow skorzystac
		mov cx, word ptr cs:[a]
		mov bx, word ptr cs:[b]
		cmp cx, bx
		jl a_less_than_b_2
		
		call get_x2 ;obliczamy x ze wzoru 2
		add ax, 160
		mov word ptr cs:[x], ax
		
		jmp a_less_than_b_2_end
		a_less_than_b_2:
		
		call get_x1 ;obliczamy x ze wzoru 1
		add ax, 160
		mov word ptr cs:[x], ax
		
		a_less_than_b_2_end:

		call draw_4_points_mirrored ; rysujemy punkty
		
		pop cx
	loop draw_ellipse_loop2

draw_ellipse_end:
	pop cx
	pop bx
	pop ax
	ret
	
get_y1: ;input - x w ax, b w bx, a w cx; output - y w ax (lepszy dla a < b)
	push dx
	
	;liczymy y = sqrt(b^2 - (x * b / a)^2)
	
	;x * b
	mul bx
	
	;x * b / a
	xor dx,dx
	div cx
	
	push cx
	
	;(x * b / a) ^2
	mul ax
	mov cx, ax
	
	;b^2
	mov ax, bx
	mul ax
	
	;(b^2 - (x * b / a)^2)
	sub ax, cx
	pop cx
	
	;sqrt(b^2 - (x * b / a)^2)
	call sqrt
	
	pop dx
	ret
	
get_y2: ;input - x w ax, b w bx, a w cx; output - y w ax (lepszy dla a > b)
	push dx
	push bx
	
	;liczymy y = b/a * sqrt(a^2 - x^2)
	
	;x^2
	mul ax
	mov bx, ax
	
	;a^2
	mov ax, cx
	mul ax
	
	;a^2 - x^2
	sub ax, bx
	
	;sqrt(a^2 - x^2)
	call sqrt
	
	;b * sqrt(a^2 - x^2)
	pop bx
	mul bx
	
	;b/a * sqrt(a^2 - x^2)
	xor dx,dx
	div cx
	
	pop dx
	ret
	
get_x1: ;input - y w ax, b w bx, a w cx; output - x w ax (lepszy dla a < b)
	push dx
	push cx
	
	;liczymy x = a/b * sqrt(b^2 - y^2)
	
	;y^2
	mul ax
	mov cx, ax
	
	;b^2
	mov ax, bx
	mul ax
	
	;b^2 - y^2
	sub ax, cx
	
	;sqrt(b^2 - y^2)
	call sqrt
	
	;a * sqrt(b^2 - y^2)
	pop cx
	mul cx
	
	;a/b * sqrt(b^2 - y^2)
	xor dx,dx
	div bx
	
	pop dx
	ret
	
get_x2: ;input - y w ax, b w bx, a w cx; output - x w ax (lepszy dla a > b)
	push dx
	
	;liczymy x = sqrt(a^2 - (y * a / b)^2)
	
	;y * a
	mul cx
	
	;y * a / b
	xor dx,dx
	div bx
	
	push bx
	
	;(y * a / b) ^2
	mul ax
	mov bx, ax
	
	;a^2
	mov ax, cx
	mul ax
	
	;(a^2 - (y * a / b) ^2)
	sub ax, bx
	pop bx
	
	;sqrt(b^2 - (x * b / a)^2)
	call sqrt
	
	pop dx
	ret
	
print: ;input - adres napisu w ds:dx
	push ax
	mov ah, 9 
	int 21h
	pop ax
	ret
	
error: ;wypisuje tekst bledu i konczy program z kodem bledu 1
	mov ax, seg data1
	mov ds, ax
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