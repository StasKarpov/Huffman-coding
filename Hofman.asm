 .386
 .model flat, stdcall
 option casemap :none
 include \masm32\include\windows.inc
 include \masm32\include\kernel32.inc
 include \masm32\include\user32.inc
 include \masm32\include\masm32.inc
 include \masm32\include\debug.inc

 includelib \masm32\lib\kernel32.lib
 includelib \masm32\lib\masm32.lib
 includelib \masm32\lib\user32.lib
 includelib \masm32\lib\debug.lib


.data

  FileName        db 128 dup (0)
  OFileName       db 128 dup (0)
  ;OFileName       db 128 dup (0)
  DecFileName     db 128 dup (0)
  DecodedFileName db 128 dup (0)

  MyByte          db 0,0


  temp            DD 0,0
  LineSize        DWORD 0 ;int
  TreeSize        DWORD 0
  pDictionary     dword 1024 dup (0)
  SumLenOfCode    dword 0
  OldTreeAdress   dword 0
  ;-------Node struct---------- 16 bytes
  ; leaf byte       - 4 bytes 11111111
  ; frequency       - 4 bytes
  ; code            - 4 bytes
  ; lenght of code  - 4 bytes
  ;
  ; so each 4 indexes we have a new letter(x4 scale)
  ;------------------------------
  pLine           dword 256  dup (0)
  pTree           dword 1020 dup (0)
  ;------tree struct-------   16 bytes
  ; 4 bytes - empty   0000000000
  ; 4 bytes - summary freq
  ; 4 bytes - adres of left child
  ; 4 bytes - adres of right child
  ;------------------------
  pDTree          dword 1020 dup (0) ; for decoding
  pDRoot          dword 0
  LeafLabel       dword 0
  pOutputFile     dword 2048 dup (0)
  pDecodedFile    dword 2048 dup (0)
  FileBuffer      DWORD 0
  OutputFileSize  dword 0
  DecodedFileSize dword 0

;-----Strings
  Greatings       db "Hello! It is Hofman Encoder/Decoder ",13,10,0
  Menu            db "Please choose the option ",13,10,0
  Encode          db "Encode (e) "  ,13,10,0
  SEncode         db "File was successfully encoded!",13,10,0
  Decode          db "Decode (d) ",13,10,0
  SDecode         db "File was successfully decoded!",13,10,0
  Quit            db "Quit   (q) " ,13,10,0
  Goodbye         db "Goodbye! ",13,10,0
  EnterEnc        db "Please enter a name of file to encode: ",13,10,0
  EnterSou        db "Please enter a name of econded file: ",13,10,0
  EnterDec        db "Pleace enter a name of file to decode: ",13,10,0

.data?

  hConsoleOut     HANDLE ?
  hInputFile      HANDLE ?
  hFileBuffer     HANDLE ?
  pFileBuffer     DWORD ? ;pointer
  hOFileBuffer    HANDLE ?
  pOFileBuffer    DWORD ? ;pointer
  hOutputFile     HANDLE ?
  hDecodedDataFile HANDLE ?



  FileSize  DWORD ? ;int



.const
  FILEBUFFERSIZE    equ 65536
  DICTIONARYSIZE    equ 4096
  LINEMAXSIZE       equ 8192
  STD_OUTPUT_HANDLE EQU -11



.code
start:
;-------------------------------interface-----------------------------------------------
   invoke StdOut, addr Greatings
Main:
   pushad


   lea  ebx,LineSize               ; making sure that all our variables have zero values
   lea  edi,DecodedFileSize
   mov  edx,0
zero_var:
   mov  [ebx],edx
   add  ebx,4
   cmp  ebx,edi
   jne  zero_var
   mov  DecodedFileSize,0


   invoke StdOut, addr Menu
   invoke StdOut, addr Encode
   invoke StdOut, addr Decode
   invoke StdOut, addr Quit
   invoke StdIn , addr MyByte,10


   cmp  MyByte,100
   je   Decoding
   cmp  MyByte,113
   je   Finish

   invoke StdOut, addr EnterEnc
   invoke StdIn , addr FileName,100





   invoke StdOut,addr EnterSou
   invoke StdIn, addr OFileName,100










;----------------------------------------------------------------------------------------








  not    LeafLabel
;---------------saving file to memory--------------------------------------------
  invoke CreateFile, addr FileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL ;opening file
  mov    hInputFile,   eax

  invoke GlobalAlloc, GMEM_MOVEABLE or GMEM_ZEROINIT, FILEBUFFERSIZE ;space alocating
  mov    hFileBuffer, eax
  invoke GlobalLock, hFileBuffer
  mov    pFileBuffer, eax


  invoke ReadFile, hInputFile, pFileBuffer, FILEBUFFERSIZE-1, addr FileSize, NULL

  invoke CloseHandle,  hInputFile; close file



;---------------adding nodes to dictionary--------------



  xor    ecx,ecx ; zero count register




  mov    esi,pFileBuffer ; esi addres of file buffer


save_to_dictionary:
  xor    edx,edx
  mov    dl,[esi+ecx]    ; get next byte from file
                         ; edx - offset/ascii name of byte
  shl    edx,1           ; edx*2
  inc    DWORD PTR pDictionary[edx*8+4] ; inc frequency
  mov    eax,LeafLabel
  mov    pDictionary[edx*8],eax


  inc    ecx             ;increment counter
  cmp    ecx,FileSize
  jne    save_to_dictionary
;------------frequency are saved-------------------


;------------insert sort to line-------------------
  xor    esi,esi         ; esi - dictionary counter(offset that we save on line)
  xor    edi,edi         ; edi - line iterator
  xor    edx,edx         ; edx - frequency that we are operating on
                         ; we will save adresess of offsets  of letters on line
                         ; f.e. a = 65(ascii number)*16(size of 1 node in dictionary)


  sub    esi,16
new_letter_from_dictionary:
  add    esi,16             ; inc dictionary counter
  cmp    esi,4096           ; check if its end of reading
  je     end_of_sorting
  mov    edi,LineSize       ; refresh line iterator
  add    edi,edi
  add    edi,edi            ; edi = edi*4

  mov    edx,pDictionary[esi+4] ; load frequency

  cmp    edx,0
  je     new_letter_from_dictionary
  lea    eax,pDictionary[esi] ; get adress
  mov    pLine[edi],eax   ; save adress on line
  inc    LineSize

line_sort:
  cmp    edi,0                    ; if it is beginning of line
  je     new_letter_from_dictionary
  sub    edi,4                    ; edi points on after-last element
                                  ; edx - still frequency of last element
  mov    eax,pLine[edi]           ; load adress
  mov    ebx,[eax+4]              ; load frequency
  mov    eax,pLine[edi+4]         ; load adress
  mov    edx,[eax+4]              ; load frequency

  cmp    ebx,edx
  jge    new_letter_from_dictionary
  mov    ebx,pLine[edi]           ; swap adresses
  mov    edx,pLine[edi+4]
  mov    pLine[edi],edx
  mov    pLine[edi+4],ebx
  jmp    line_sort
end_of_sorting:




;---------we have sorted line-----------------

;---------building tree------
  xor    ecx,ecx                  ; ecx - tree iterator
  xor    edi,edi                  ; edi - line iterator
  xor    edx,edx                  ; edx - frequency on which we are operating


build_node:

  mov    edi,LineSize             ; refresh line iterator
  cmp    edi,1
  je     end_of_building_tree
  add    edi,edi
  add    edi,edi                  ; edi*=4
  sub    edi,4                    ; edi points on last element
  sub    edi,4                    ; edi points after last element
  mov    eax,pLine[edi]           ; eax - adress of left son
  mov    edx,[eax+4]              ; load frequency of left son
  mov    pTree[ecx+8],eax         ; save adress of left son in tree node
  mov    eax,pLine[edi+4]         ; eax - adress of right son
  add    edx,[eax+4]              ; add frequency of right son to left frequency
  mov    pTree[ecx+12],eax        ; save adress of right son in tree node
  mov    pTree[ecx+4],edx         ; save summary frequency in tree node
  dec    LineSize                 ; edi points on last element
  lea    eax,pTree[ecx]           ; load adress of three node
  mov    pLine[edi],eax           ; put this adress on line
  add    ecx,16                   ; inc tree iterator
  inc    TreeSize
sort_node:
  cmp    edi,0                    ; if it is beginning of line
  je     build_node
  sub    edi,4                    ; edi points on after-last element
                                  ; edx - still frequency of last element
  mov    eax,pLine[edi]           ; load adress
  mov    ebx,[eax+4]              ; load frequency
  mov    eax,pLine[edi+4]         ; load adress
  mov    edx,[eax+4]              ; load frequency

  cmp    ebx,edx
  jge    build_node
  mov    ebx,pLine[edi]           ; swap adresses
  mov    edx,pLine[edi+4]
  mov    pLine[edi],edx
  mov    pLine[edi+4],ebx
  jmp    sort_node
end_of_building_tree:


;------------tree was built------------
;------------trawersing tree and building codes-------




  xor    ecx,ecx     ; ecx - lenght of codes
  xor    edx,edx     ; ebx - adress of parent root
  xor    esi,esi     ; esi - code

  mov    edx,pLine   ;load root of tree

  call   code
  jmp    end_of_coding

code:
  mov    eax,[edx] ; load leaf 4 bytes
  cmp    eax,LeafLabel ; if its leaf
  je     save_code
  mov    eax,[edx+8] ; load adress of left son
  cmp    eax,0
  je     check_right ; if it is no left son then check right son
  push   edx            ; prepering to visit left son
  mov    edx,[edx+8]
  shl    esi,1
  inc    ecx            ; inc lenght of code
  call   code

check_right:
  mov    eax,[edx+12]    ; load adress of rigth son
  cmp    eax,0
  je     end_of_rec
  push   edx            ; prepering to visit right son
  mov    edx,[edx+12]
  shl    esi,1
  inc    esi            ; add 1 to code
  inc    ecx            ; inc lenght of code
  call   code


end_of_rec:
  pop    eax             ; load return adress
  pop    edx             ; load parent
  dec    ecx             ; dec lenght
  shr    esi,1             ; dec code
  push   eax
  ret

save_code:
             ; how are we coming here when this is nonleaf node ????
  mov    [edx+12],ecx    ; save lenght of code
  mov    [edx+8],esi     ; save code

  dec    ecx             ; dec lenght
  shr    esi,1             ; dec code
  pop    eax             ; load adress where to return
  pop    edx             ; load parent
  push   eax
  ret

end_of_coding:


;----------------write data to file --------------------
  xor    ecx,ecx         ; output file iterator
  mov    eax,FileSize
  mov    pOutputFile,eax        ; put size of bytes in file
  add    ecx,4
  mov    eax,TreeSize
  mov    pOutputFile[ecx],eax   ; put quantity of nonleaf nodes
  lea    eax,pTree
  add    ecx,4
  mov    pOutputFile[ecx],eax  ; put old adress of tree
  add    ecx,8                 ; 8 not 4 to keep it align

  ; writing tree strukture to file
  ; if adress poitnts on dictionary we translate it to offset
  ; if adress poitns  on tree we leave it as it is



  xor   edi,edi ; edi - tree counter
write_tree:
  mov   edx,pTree[edi+8] ; load left child adress
  mov   eax,[edx]       ; load leaf label
  cmp   eax,LeafLabel   ; if it is leaf
  jne   end_of_translating_left
                        ; here we translate adress
  lea   eax,pDictionary
  sub   edx,eax         ; get offset
end_of_translating_left:
  mov   pOutputFile[ecx],edx ; save adress or offset
  mov   edx,pTree[edi+12]  ; load right child adress
  mov   eax,[edx]         ; load leaf byte
  cmp   eax,LeafLabel
  jne   end_of_transating_right
  lea   eax,pDictionary
  sub   edx,eax         ; get offset
end_of_transating_right:
  mov   pOutputFile[ecx+4],edx; save stuff
  add   edi,16             ; inc tree counter
  add   ecx,8              ; inc file counter
  mov   eax,TreeSize
  shl   eax,4              ; TreeSize*=16
  cmp   eax,edi
  jne   write_tree


;----------now we will write codings
  xor  edi,edi         ; edi =input file iterator
  mov  esi,pFileBuffer ; esi base adress or buffer for lenght
                       ; SumLenOfCode = summary lenght of code
                       ; ecx = output  file iterator
  xor  ebx,ebx         ; ebx = code buffer
get_code:
  cmp  FileSize,edi
  je   end_of_saving_codes
  xor  eax,eax
  mov  esi,pFileBuffer
  mov  al,[esi+edi]    ; load byte
  shl  eax,4           ; get offset
  mov  esi,pDictionary[eax+12] ; load lenght of code
  add  SumLenOfCode,esi
  cmp  SumLenOfCode,32
  jge  write_code
  push eax
  xchg esi,ecx
  shl  ebx,cl    ; reserve place for coding
  xchg esi,ecx
  pop  eax
  add  ebx,pDictionary[eax+8] ; add code
  inc  edi                ; inc input file iterator
  jmp  get_code

write_code:
  sub  SumLenOfCode,32    ; refresh summary length of code
  push ecx     ; save ecx value
  mov  ecx,SumLenOfCode
  mov  esi,pDictionary[eax+12] ; load lenght
  mov  eax,pDictionary[eax+8] ; load code
  push eax     ; save code
  shr  eax,cl  ; delete bits from code
  sub  esi,SumLenOfCode ; esi how mush bits we can add to old buffer
  mov  ecx,esi
  shl  ebx,cl     ; reserve this much bits
  add  ebx,eax    ; add coding
                  ; ebx is full
  pop  eax
  pop  ecx        ; ecx - output file iterator
  mov  pOutputFile[ecx],ebx ;save code buffer
  add  ecx,4                ; inc output file iterator
  xor  ebx,ebx
  add  ebx,eax ; add rest of codings

  xor  eax,eax
  inc  eax
  push ecx
  mov  esi,SumLenOfCode
  mov  ecx,esi
  shl  eax,cl
  sub  eax,1


  and  ebx,eax  ; leave only bits that wil add
  pop  ecx

  inc  edi      ; inc input file iterator
  jmp  get_code
end_of_saving_codes:
                ; save whats left in buffer
  push ecx
  mov  ecx,32
  sub  ecx,SumLenOfCode
  shl  ebx,cl
  pop  ecx
  mov  pOutputFile[ecx],ebx
  add  ecx,4
  mov  OutputFileSize,ecx
;-------write all stuff to file

  invoke CreateFile, addr OFileName, GENERIC_WRITE, FILE_SHARE_WRITE, NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL ;opening file
  mov    hOutputFile,   eax

  invoke WriteFile, hOutputFile,addr pOutputFile, OutputFileSize, NULL, NULL

  invoke CloseHandle,  hOutputFile; close file


  invoke GlobalUnlock, pFileBuffer
  invoke GlobalFree,   hFileBuffer
  invoke StdOut,addr SEncode
  popad
jmp Main

Decoding:
;_________________________________________________________________________________________________________________________________________
;---------------DECODER

  invoke StdOut,addr EnterDec
  invoke StdIn ,addr DecFileName,100

  invoke CreateFile, addr DecFileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL ;opening file
  mov    hInputFile,   eax

  invoke GlobalAlloc, GMEM_MOVEABLE or GMEM_ZEROINIT, FILEBUFFERSIZE ;space alocating
  mov    hOFileBuffer, eax
  invoke GlobalLock, hOFileBuffer
  mov    pOFileBuffer, eax

  invoke ReadFile, hInputFile, pOFileBuffer, FILEBUFFERSIZE-1, NULL, NULL

  invoke CloseHandle,  hInputFile; close file

;--- we have our encoded data in pFileBuffer

  xor  ecx,ecx  ; input file counter
  mov  esi,pOFileBuffer
  mov  eax,[esi] ; load size of file
  mov  FileSize,eax
  add  ecx,4
  mov  eax,[esi+ecx] ; load quantity of non leaf nodes
  mov  TreeSize,eax
  add  ecx,4
  mov  eax,[esi+ecx] ; load old adress of tree
  mov  OldTreeAdress,eax
  add  ecx,8
; now loading tree
; if adress < 16*256=4096 => it is leaf leave it
; if        > it s tree node and we also make new adress


  xor  edi,edi  ; edi decoded tree itrerator
  xor  eax,eax
  mov  esi,pOFileBuffer

load_tree:


  mov  eax,[esi+ecx] ; load left adress

  cmp  eax,6000        ; check is it a laaf
  jng  write_left

  sub  eax,OldTreeAdress                 ; make new tree adress
  lea  edx,pDTree
  add  eax,edx       ; edx new tree adress
write_left:
  mov  pDTree[edi+8],eax ; save left son


  mov  eax,[esi+ecx+4]  ; load right son adress
  cmp  eax,6000        ; check is it a laaf

  jng  write_right
  sub  eax,OldTreeAdress                 ; make new tree adress
  lea  edx,pDTree
  add  eax,edx       ; edx new tree adress
write_right:
  mov  pDTree[edi+12],eax ; save left son

  add  edi,16               ; inc tree iterator
  add  ecx,8              ; inc input file iterator
  mov  eax,TreeSize
  shl  eax,4            ; eax*=16
  cmp  edi,eax     ; end of reading decoded tree
  jne  load_tree

  sub  eax,16 ; load root node to edi
  lea  eax,pDTree[eax]
  mov  pDRoot,eax ; save adress of root


;-----------------new tree created
; know lets travel and decode text
               ; esi - pFileBuffer
               ; ecx - pFileBuffer counter
               ; edi - tree iterator, now it is root node
  mov edi,pDRoot
  sub ecx,4
load_block:
  add  ecx,4
  mov  edx,[esi+ecx] ; load block of bytes
  mov  ebx,1         ; refresh mask  00000...0001
  xor  eax,eax

load_byte:
  ror  ebx,1         ; start with 10000...00
  mov  eax,edx       ; load block of bytes
  and  eax,ebx       ; take byte
  cmp  eax,0
  je   go_left
                     ; else go right
  mov  edi,[edi+12]  ; load right son
  jmp  after_left

go_left:
  mov edi,[edi+8]   ; load left son

after_left:
  cmp edi,6000      ; iff it is leaf we save it
  jl  save_letter
  cmp ebx,1         ; if mask = 0000000...01
  je  load_block
  jmp load_byte

save_letter:
  shr edi,4

  ;mov MyByte,edi

  push eax
  mov  eax,DecodedFileSize    ; save letter to decoded file
  mov  pDecodedFile[eax],edi
  inc  eax
  mov  DecodedFileSize,eax
  pop  eax

  dec FileSize

  cmp FileSize,0
  je  end_of_decoding

  mov edi,pDRoot

  cmp ebx,1         ; if mask = 0000000...01
  je  load_block
  jmp load_byte


end_of_decoding:
;
;--------------------output decoded data---------------------------------
;DecFileName - name of file we just decoded
  lea ebx,DecFileName
  lea eax,DecodedFileName

read_name:
  mov dl,[ebx]
  cmp dl,46 ; check if it is dot
  je  end_of_read_name
  mov [eax],dl
  inc eax
  inc ebx
  jmp read_name
end_of_read_name:


  mov dl,68
  mov [eax],dl ; save letter D
  mov dl,46
  mov [eax+1],dl
  mov dl,116
  mov [eax+2],dl
  mov dl,120
  mov [eax+3],dl
  mov dl,116
  mov [eax+4],dl



invoke CreateFile, addr DecodedFileName, GENERIC_WRITE, FILE_SHARE_WRITE, NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL ;opening file
mov    hOutputFile,   eax

invoke WriteFile, hOutputFile,addr pDecodedFile, DecodedFileSize, NULL, NULL

invoke CloseHandle,  hOutputFile; close file




  ;  PrintDec TreeSize,"Size of tree: "
   ;
  ;  PrintText "Tree map"
  ;  lea    edx,pTree
  ;  DbgDump edx,1028
  ;  PrintText "New Tree map"
  ;  lea    edx,pDTree
  ;  DbgDump edx,1028
  ;  PrintText "pOutputFile map"
  ;  lea    edx,pOutputFile
  ;  DbgDump edx,564
   ;
  ;  PrintText "/n"
  ;  PrintText "Dictioanry Map"
  ;  lea    edx,pDictionary
  ;  DbgDump edx,2024
  invoke GlobalUnlock, pOFileBuffer
  invoke GlobalFree,   hOFileBuffer
;----------- exit process -------------------------
invoke StdOut,addr SDecode
popad
jmp Main


Finish:
invoke ExitProcess,  NULL



end start
