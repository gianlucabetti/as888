000100031205      *------------------------------------------------------------------------*
000200100108      * Tassazione bolle mondoffice
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600100111     ffivab01l  if a e           k Disk
000700100113     fwfmon00f  if a e           k Disk
000800100113     Fsep20093rdiF   E             DISK    rename(sep20093rd:sep2009x00)
000900100112     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000031205
001100100108     D DSsep           DS
001200100113     d  I_inutile              1     44
001300100113     d  I_tipospe             45     64
001400100113     d  I_datspe              65     74
001500100113     d  I_cad6vuoto           75     80
001600100113     d  I_cad5                81     85
001700100113     d  I_cad9                86     94
001800100113     d  I_cad5vuoto           95     99
001900100113     d  I_numcli             100    112
002000100113     d  I_rmn                113    125
002100100113     d  I_vuoto2             126    126
002200100113     d  I_colli              127    136
002300100113     d  I_vuoto              137    139
002400100113     d  I_parcel             140    147
002500100113     d  I_volume             148    162
002600100113     d  I_peso               163    176
002700100113     d  I_lod                177    210
002800100113     d  I_prd                211    212
002900080103     d
003000100108     d savrmn          s                   like(i_rmn)
003100100108     d savlod          s                   like(i_lod)
003200100111     d totpes          s             15  0
003300100111     d totvol          s             15  0
003400100112     d totcol          s             15  0
003500100111     d totncl          s              5  0
003600100111     D CMAIUS          C                   CONST('ABCDEFGHIJKLMNOPQRS-
003700100111     D                                     TUVWYXZ')
003800100111     D CMINUS          C                   CONST('abcdefghijklmnopqrs-
003900100111     D                                     tuvwyxz')
004000100111     isep2009x00
004100100113     I              sep20093rd                  sep2009fld
004200071122      *------------------------------------------------------------------------*
004300100111     C                   MOVEL     CMAIUS        MA               26
004400100111     C                   MOVEL     CMINUS        MI               26
004500071116      /free
004600071122       *inlr = *on   ;
004700071122
004800100113       read sep20093rd ;
004900100113       read sep20093rd ;
005000071116
0051001001131      dow  not %eof(sep20093rd);
005200080103
005300100111       dssep=sep2009fld    ;
005400100113
005500100113       if i_rmn<>*blANKS   ;
005600100108
005700100111       // memorizzo numero ordine a rottura  scrivo fivab
005800100111       if   savrmn=*blanks  ;
005900100111         savrmn=i_rmn   ;
006000100111         savlod=i_lod   ;
006100100108       endif    ;
006200100108
006300100111       if   savrmn<>i_rmn or  savlod<>i_lod   ;
006400100112       eval(H) vabpkb=totpes/1000   ;
006500100112       eval(H) vabvlb=totvol/1000000000 ;
006600100112
006700100112       // Se i colli non corrispondono stampo errore e non scrivo
006800100111
006900100112       if totcol=vabncl  ;
007000100111       write fivab000    ;
007100100112       else   ;
007200100112       except errore  ;
007300100112       endif  ;
007400100111
007500100111       clear fivab000    ;
007600100111       clear totpes  ;
007700100111       clear totvol  ;
007800100112         savrmn=i_rmn   ;
007900100112         savlod=i_lod   ;
008000100111       endif    ;
008100100111
008200100111       if   savrmn=i_rmn and savlod=i_lod   ;
008300100111
008400100111         i_peso=%xlate(' ':'0':i_peso) ;
008500100111         totpes= totpes+ %int(i_peso)  ;
008600100111
008700100111         i_volume=%xlate(' ':'0':i_volume) ;
008800100111         totvol= totvol+ %int(i_volume)  ;
008900100111
009000100111         exsr elabora   ;
009100100111       endif    ;
009200100113       endif    ;
009300080103
009400100113       read sep20093rd ;
009500100111       enddo    ;
009600100112
009700100112       // Ultima scrittura
009800100112       if vabncl>0   ;
009900100112       eval(H) vabpkb=totpes/1000   ;
010000100112       eval(H) vabvlb=totvol/1000000000 ;
010100100112
010200100113       if totcol=vabncl  ;
010300100113       write fivab000    ;
010400100113       else   ;
010500100113       except errore  ;
010600100113       endif  ;
010700100112       endif   ;
010800100112
010900080107
011000100111       //------------------------------------------/
011100100111       BEGSR ELABORA   ;
011200100111
011300100111       vabccm=0493666   ;
011400100112       vabfgs=049       ;
011500100112       vablnp=049       ;
011600100112       vabaas=2009      ;
011700100113       vabctm='7 '      ;
011800100111       vabcbo='1 '      ;
011900100111       vabrsd='PROVA FATTURAZ. '   ;
012000100111       vabIND='SETTEMBRE 2009'   ;
012100100112       i_cad5=%xlate(' ':'0':i_cAD5) ;
012200100112       vabCAD=I_CAD5     ;
012300100111       vabLOD=I_LOD      ;
012400100111       vablod  =%xlate(mi:ma:vablod) ;
012500100111       vabprd=I_prd      ;
012600100111       vabnas='FATTURAZ' ;
012700100111       vabncl=vabncl +1  ;
012800100111
012900100111       i_rmn   =%xlate(' ':'0':i_rmn) ;
013000100111       vabrmn=%int(i_rmn)      ;
013100100113       vabrma='SEP20093RD';
013200100112
013300100113       // Quando scrivo il primo collo, scrivo anche il file di output
013400100112       if vabncl=1   ;
013500100112       clear wfmond00   ;
013600100112
013700100113       mon_file='D'    ;
013800100113       mon_rmn='0'+i_rmn  ;
013900100113       mon_descr=i_inutile   ;
014000100113       mon_dsped=i_tipospe  ;
014100100113       mon_dsp  =i_datspe    ;
014200100113       mon_cad  =i_cad9+i_cad5;
014300100113       mon_cli  =i_numcli       ;
014400100113       mon_colli=i_colli     ;
014500100113       mon_lod  =i_lod       ;
014600100113       mon_prd  =i_prd       ;
014700100112       write wfmond00    ;
014800100112       endif   ;
014900100113
015000100113       i_colli =%xlate(' ':'0':i_colli);
015100100113       totcol= %int(i_colli)  ;
015200100112
015300100111       ENDSR   ;
015400100111
015500071116      /end-free
015600100112     OQSYSPRT   E            errore      1
015700100113     O                                              'FILE "D" : '
015800100113     O                                        +   0 'I colli non tornano!! RMN'
015900100112     O                       vabrmn           +   1
016000100112     O                                        +   2 'Cad'
016100100112     O                       vabCAD           +   1
016200100112     O                                        +   2 'Lod'
016300100112     O                       vablod           +   1
016400100112     O                                        +   2 'Prd'
016500100112     O                       vabprd           +   1
016600100112     OQSYSPRT   E            errore      1
016700100112     O                                        +  30 'Ncl'
016800100112     O                       vabncl        2  +   1
016900100113     O                                        +   3 'Colli da cli'
017000100112     O                       totcol        2  +   1
