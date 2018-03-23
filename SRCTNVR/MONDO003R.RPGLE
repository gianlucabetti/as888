000100031205      *------------------------------------------------------------------------*
000200100108      * Tassazione bolle mondoffice
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600100111     ffivab01l  if a e           k Disk
000700100113     fwfmon00f  if a e           k Disk
000800100113     Fsep20092ndiF   E             DISK    rename(sep20092nd:sep2009x00)
000900100112     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000031205
001100100108     D DSsep           DS
001200100113     d  I_inutile              1     43
001300100113     d  I_tipospe             44     69
001400100113     d  I_datspe              70     79
001500100113     d  I_cad5                80     84
001600100113     d  I_cad9                85     93
001700100113     d  I_numcli              94    106
001800100113     d  I_rmn                107    118
001900100113     d  I_colli              119    128
002000100113     d  I_vuoto              129    131
002100100113     d  I_parcel             132    139
002200100113     d  I_volume             140    154
002300100113     d  I_peso               155    166
002400100113     d  I_lod                167    203
002500100113     d  I_prd                204    205
002600080103     d
002700100108     d savrmn          s                   like(i_rmn)
002800100108     d savlod          s                   like(i_lod)
002900100111     d totpes          s             15  0
003000100111     d totvol          s             15  0
003100100112     d totcol          s             15  0
003200100111     d totncl          s              5  0
003300100111     D CMAIUS          C                   CONST('ABCDEFGHIJKLMNOPQRS-
003400100111     D                                     TUVWYXZ')
003500100111     D CMINUS          C                   CONST('abcdefghijklmnopqrs-
003600100111     D                                     tuvwyxz')
003700100111     isep2009x00
003800100113     I              sep20092nd                  sep2009fld
003900071122      *------------------------------------------------------------------------*
004000100111     C                   MOVEL     CMAIUS        MA               26
004100100111     C                   MOVEL     CMINUS        MI               26
004200071116      /free
004300071122       *inlr = *on   ;
004400071122
004500100113       read sep20092nd ;
004600100113       read sep20092nd ;
004700071116
0048001001131      dow  not %eof(sep20092nd);
004900080103
005000100111       dssep=sep2009fld    ;
005100100113
005200100113       if i_rmn<>*blANKS   ;
005300100108
005400100111       // memorizzo numero ordine a rottura  scrivo fivab
005500100111       if   savrmn=*blanks  ;
005600100111         savrmn=i_rmn   ;
005700100111         savlod=i_lod   ;
005800100108       endif    ;
005900100108
006000100111       if   savrmn<>i_rmn or  savlod<>i_lod   ;
006100100112       eval(H) vabpkb=totpes/1000   ;
006200100112       eval(H) vabvlb=totvol/1000000000 ;
006300100112
006400100112       // Se i colli non corrispondono stampo errore e non scrivo
006500100111
006600100112       if totcol=vabncl  ;
006700100111       write fivab000    ;
006800100112       else   ;
006900100112       except errore  ;
007000100112       endif  ;
007100100111
007200100111       clear fivab000    ;
007300100111       clear totpes  ;
007400100111       clear totvol  ;
007500100112         savrmn=i_rmn   ;
007600100112         savlod=i_lod   ;
007700100111       endif    ;
007800100111
007900100111       if   savrmn=i_rmn and savlod=i_lod   ;
008000100111
008100100111         i_peso=%xlate(' ':'0':i_peso) ;
008200100111         totpes= totpes+ %int(i_peso)  ;
008300100111
008400100111         i_volume=%xlate(' ':'0':i_volume) ;
008500100111         totvol= totvol+ %int(i_volume)  ;
008600100111
008700100111         exsr elabora   ;
008800100111       endif    ;
008900100113       endif    ;
009000080103
009100100113       read sep20092nd ;
009200100111       enddo    ;
009300100112
009400100112       // Ultima scrittura
009500100112       if vabncl>0   ;
009600100112       eval(H) vabpkb=totpes/1000   ;
009700100112       eval(H) vabvlb=totvol/1000000000 ;
009800100112
009900100113       if totcol=vabncl  ;
010000100113       write fivab000    ;
010100100113       else   ;
010200100113       except errore  ;
010300100113       endif  ;
010400100112       endif   ;
010500100112
010600080107
010700100111       //------------------------------------------/
010800100111       BEGSR ELABORA   ;
010900100111
011000100111       vabccm=0493666   ;
011100100112       vabfgs=049       ;
011200100112       vablnp=049       ;
011300100112       vabaas=2009      ;
011400100113       vabctm='7 '      ;
011500100111       vabcbo='1 '      ;
011600100111       vabrsd='PROVA FATTURAZ. '   ;
011700100111       vabIND='SETTEMBRE 2009'   ;
011800100112       i_cad5=%xlate(' ':'0':i_cAD5) ;
011900100112       vabCAD=I_CAD5     ;
012000100111       vabLOD=I_LOD      ;
012100100111       vablod  =%xlate(mi:ma:vablod) ;
012200100111       vabprd=I_prd      ;
012300100111       vabnas='FATTURAZ' ;
012400100111       vabncl=vabncl +1  ;
012500100111
012600100111       i_rmn   =%xlate(' ':'0':i_rmn) ;
012700100111       vabrmn=%int(i_rmn)      ;
012800100113       vabrma='SEP20092ND';
012900100112
013000100113       // Quando scrivo il primo collo, scrivo anche il file di output
013100100112       if vabncl=1   ;
013200100112       clear wfmond00   ;
013300100112
013400100113       mon_file='C'    ;
013500100113       mon_rmn='00'+i_rmn  ;
013600100113       mon_descr=i_inutile   ;
013700100113       mon_dsped=i_tipospe  ;
013800100113       mon_dsp  =i_datspe    ;
013900100113       mon_cad  =i_cad9+i_cad5;
014000100113       mon_cli  =i_numcli       ;
014100100113       mon_colli=i_colli     ;
014200100113       mon_lod  =i_lod       ;
014300100113       mon_prd  =i_prd       ;
014400100112       write wfmond00    ;
014500100112       endif   ;
014600100113
014700100113       i_colli =%xlate(' ':'0':i_colli);
014800100113       totcol= %int(i_colli)  ;
014900100112
015000100111       ENDSR   ;
015100100111
015200071116      /end-free
015300100112     OQSYSPRT   E            errore      1
015400100113     O                                              'FILE "C" : '
015500100113     O                                        +   0 'I colli non tornano!! RMN'
015600100112     O                       vabrmn           +   1
015700100112     O                                        +   2 'Cad'
015800100112     O                       vabCAD           +   1
015900100112     O                                        +   2 'Lod'
016000100112     O                       vablod           +   1
016100100112     O                                        +   2 'Prd'
016200100112     O                       vabprd           +   1
016300100112     OQSYSPRT   E            errore      1
016400100112     O                                        +  30 'Ncl'
016500100112     O                       vabncl        2  +   1
016600100113     O                                        +   3 'Colli da cli'
016700100112     O                       totcol        2  +   1
