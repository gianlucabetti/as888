000100031205      *------------------------------------------------------------------------*
000200100108      * Tassazione bolle mondoffice
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600100111     ffivab01l  if a e           k Disk
000700100113     fwfmon00f  if a e           k Disk
000800100113     Fsep20091stiF   E             DISK    rename(sep20091st:sep2009x00)
000900100112     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000031205
001100100108     D DSsep           DS
001200100113     d  I_inutile              1     43
001300100113     d  I_tipospe             44     68
001400100113     d  I_datspe              69     78
001500100113     d  I_cad5                79     83
001600100113     d  I_cad9                84     92
001700100113     d  I_cad5b               93     97
001800100113     d  I_numcli              98    110
001900100113     d  I_rmn                111    124
002000100113     d  I_colli              125    134
002100100113     d  I_vuoto              135    137
002200100113     d  I_parcel             138    145
002300100113     d  I_volume             146    159
002400100113     d  I_peso               160    174
002500100113     d  I_lod                175    208
002600100113     d  I_prd                209    210
002700080103     d
002800100108     d savrmn          s                   like(i_rmn)
002900100108     d savlod          s                   like(i_lod)
003000100111     d totpes          s             15  0
003100100111     d totvol          s             15  0
003200100112     d totcol          s             15  0
003300100111     d totncl          s              5  0
003400100111     D CMAIUS          C                   CONST('ABCDEFGHIJKLMNOPQRS-
003500100111     D                                     TUVWYXZ')
003600100111     D CMINUS          C                   CONST('abcdefghijklmnopqrs-
003700100111     D                                     tuvwyxz')
003800100111     isep2009x00
003900100113     I              sep20091st                  sep2009fld
004000071122      *------------------------------------------------------------------------*
004100100111     C                   MOVEL     CMAIUS        MA               26
004200100111     C                   MOVEL     CMINUS        MI               26
004300071116      /free
004400071122       *inlr = *on   ;
004500071122
004600100113       read sep20091st ;
004700100113       read sep20091st ;
004800071116
0049001001131      dow  not %eof(sep20091st);
005000080103
005100100111       dssep=sep2009fld    ;
005200100113
005300100113       if i_rmn<>*blANKS   ;
005400100108
005500100111       // memorizzo numero ordine a rottura  scrivo fivab
005600100111       if   savrmn=*blanks  ;
005700100111         savrmn=i_rmn   ;
005800100111         savlod=i_lod   ;
005900100108       endif    ;
006000100108
006100100111       if   savrmn<>i_rmn or  savlod<>i_lod   ;
006200100112       eval(H) vabpkb=totpes/1000   ;
006300100112       eval(H) vabvlb=totvol/1000000000 ;
006400100112
006500100112       // Se i colli non corrispondono stampo errore e non scrivo
006600100111
006700100112       if totcol=vabncl  ;
006800100111       write fivab000    ;
006900100112       else   ;
007000100112       except errore  ;
007100100112       endif  ;
007200100111
007300100111       clear fivab000    ;
007400100111       clear totpes  ;
007500100111       clear totvol  ;
007600100112         savrmn=i_rmn   ;
007700100112         savlod=i_lod   ;
007800100111       endif    ;
007900100111
008000100111       if   savrmn=i_rmn and savlod=i_lod   ;
008100100111
008200100111         i_peso=%xlate(' ':'0':i_peso) ;
008300100111         totpes= totpes+ %int(i_peso)  ;
008400100111
008500100111         i_volume=%xlate(' ':'0':i_volume) ;
008600100111         totvol= totvol+ %int(i_volume)  ;
008700100111
008800100111         exsr elabora   ;
008900100111       endif    ;
009000100113       endif    ;
009100080103
009200100113       read sep20091st ;
009300100111       enddo    ;
009400100112
009500100112       // Ultima scrittura
009600100112       if vabncl>0   ;
009700100112       eval(H) vabpkb=totpes/1000   ;
009800100112       eval(H) vabvlb=totvol/1000000000 ;
009900100112
010000100113       if totcol=vabncl  ;
010100100113       write fivab000    ;
010200100113       else   ;
010300100113       except errore  ;
010400100113       endif  ;
010500100112       endif   ;
010600100112
010700080107
010800100111       //------------------------------------------/
010900100111       BEGSR ELABORA   ;
011000100111
011100100111       vabccm=0493666   ;
011200100112       vabfgs=049       ;
011300100112       vablnp=049       ;
011400100112       vabaas=2009      ;
011500100112       vabctm='6 '      ;
011600100111       vabcbo='1 '      ;
011700100111       vabrsd='PROVA FATTURAZ. '   ;
011800100111       vabIND='SETTEMBRE 2009'   ;
011900100112       i_cad5=%xlate(' ':'0':i_cAD5) ;
012000100112       vabCAD=I_CAD5     ;
012100100111       vabLOD=I_LOD      ;
012200100111       vablod  =%xlate(mi:ma:vablod) ;
012300100111       vabprd=I_prd      ;
012400100111       vabnas='FATTURAZ' ;
012500100111       vabncl=vabncl +1  ;
012600100111
012700100111       i_rmn   =%xlate(' ':'0':i_rmn) ;
012800100111       vabrmn=%int(i_rmn)      ;
012900100113       vabrma='SEP20091ST';
013000100112
013100100113       // Quando scrivo il primo collo, scrivo anche il file di output
013200100112       if vabncl=1   ;
013300100112       clear wfmond00   ;
013400100112
013500100113       mon_file='B'    ;
013600100113       mon_rmn=i_rmn  ;
013700100113       mon_descr=i_inutile   ;
013800100113       mon_dsped=i_tipospe  ;
013900100113       mon_dsp  =i_datspe    ;
014000100113       mon_cad  =i_cad9+i_cad5;
014100100113       mon_cli  =i_numcli       ;
014200100113       mon_colli=i_colli     ;
014300100113       mon_lod  =i_lod       ;
014400100113       mon_prd  =i_prd       ;
014500100112       write wfmond00    ;
014600100112       endif   ;
014700100113
014800100113       i_colli =%xlate(' ':'0':i_colli);
014900100113       totcol= %int(i_colli)  ;
015000100112
015100100111       ENDSR   ;
015200100111
015300071116      /end-free
015400100112     OQSYSPRT   E            errore      1
015500100112     O                                              'I colli non tornano!! RMN'
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
