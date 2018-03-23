000100031205      *------------------------------------------------------------------------*
000200100108      * Tassazione bolle mondoffice
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600100111     ffivab01l  if a e           k Disk
000700100112     fwfmon00f  if a e           k Disk
000800100111     Fsep2009xxxiF   E             DISK    rename(sep2009xxx:sep2009x00)
000900100112     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000031205
001100100108     D DSsep           DS
001200100108     d  I_inutile              1     41
001300100108     d  I_tipospe             42     62
001400100108     d  I_datspe              63     73
001500100111     d  I_cad9                74     82
001600100111     d  I_cad5                83     87
001700100108     d  I_numcli              88    100
001800100108     d  I_rmn                101    114
001900100112     d  I_colli              115    124
002000100112     d  I_parcel             125    132
002100100108     d  I_volume             133    145
002200100108     d  I_peso               146    157
002300100108     d  I_lod                158    198
002400100108     d  I_prd                199    200
002500080103     d
002600100108     d savrmn          s                   like(i_rmn)
002700100108     d savlod          s                   like(i_lod)
002800100111     d totpes          s             15  0
002900100111     d totvol          s             15  0
003000100112     d totcol          s             15  0
003100100111     d totncl          s              5  0
003200100111     D CMAIUS          C                   CONST('ABCDEFGHIJKLMNOPQRS-
003300100111     D                                     TUVWYXZ')
003400100111     D CMINUS          C                   CONST('abcdefghijklmnopqrs-
003500100111     D                                     tuvwyxz')
003600100111     isep2009x00
003700100111     I              sep2009xxx                  sep2009fld
003800071122      *------------------------------------------------------------------------*
003900100111     C                   MOVEL     CMAIUS        MA               26
004000100111     C                   MOVEL     CMINUS        MI               26
004100071116      /free
004200071122       *inlr = *on   ;
004300071122
004400100108       read sep2009xxx ;
004500100112       read sep2009xxx ;
004600071116
0047001001081      dow  not %eof(sep2009xxx);
004800080103
004900100111       dssep=sep2009fld    ;
005000100108
005100100111       // memorizzo numero ordine a rottura  scrivo fivab
005200100111       if   savrmn=*blanks  ;
005300100111         savrmn=i_rmn   ;
005400100111         savlod=i_lod   ;
005500100108       endif    ;
005600100108
005700100111       if   savrmn<>i_rmn or  savlod<>i_lod   ;
005800100112       eval(H) vabpkb=totpes/1000   ;
005900100112       eval(H) vabvlb=totvol/1000000000 ;
006000100112
006100100112       // Se i colli non corrispondono stampo errore e non scrivo
006200100111
006300100112       if totcol=vabncl  ;
006400100111       write fivab000    ;
006500100112       else   ;
006600100112       except errore  ;
006700100112       endif  ;
006800100111
006900100111       clear fivab000    ;
007000100111       clear totpes  ;
007100100111       clear totvol  ;
007200100112         savrmn=i_rmn   ;
007300100112         savlod=i_lod   ;
007400100111       endif    ;
007500100111
007600100111       if   savrmn=i_rmn and savlod=i_lod   ;
007700100111
007800100111         i_peso=%xlate(' ':'0':i_peso) ;
007900100111         totpes= totpes+ %int(i_peso)  ;
008000100111
008100100111         i_volume=%xlate(' ':'0':i_volume) ;
008200100111         totvol= totvol+ %int(i_volume)  ;
008300100111
008400100111         exsr elabora   ;
008500100111       endif    ;
008600080103
008700100111       read sep2009xxx ;
008800100111       enddo    ;
008900100112
009000100112       // Ultima scrittura
009100100112       if vabncl>0   ;
009200100112       eval(H) vabpkb=totpes/1000   ;
009300100112       eval(H) vabvlb=totvol/1000000000 ;
009400100112
009500100112       write fivab000    ;
009600100112       endif   ;
009700100112
009800080107
009900100111       //------------------------------------------/
010000100111       BEGSR ELABORA   ;
010100100111
010200100111       vabccm=0493666   ;
010300100112       vabfgs=049       ;
010400100112       vablnp=049       ;
010500100112       vabaas=2009      ;
010600100112       vabctm='6 '      ;
010700100111       vabcbo='1 '      ;
010800100111       vabrsd='PROVA FATTURAZ. '   ;
010900100111       vabIND='SETTEMBRE 2009'   ;
011000100112       i_cad5=%xlate(' ':'0':i_cAD5) ;
011100100112       vabCAD=I_CAD5     ;
011200100111       vabLOD=I_LOD      ;
011300100111       vablod  =%xlate(mi:ma:vablod) ;
011400100111       vabprd=I_prd      ;
011500100111       vabnas='FATTURAZ' ;
011600100111       vabncl=vabncl +1  ;
011700100111
011800100111       i_rmn   =%xlate(' ':'0':i_rmn) ;
011900100111       vabrmn=%int(i_rmn)      ;
012000100112       vabrma='SEP2009XXX';
012100100112
012200100113       // Quando scrivo il primo collo, scrivo anche il file di output
012300100112       if vabncl=1   ;
012400100112       clear wfmond00   ;
012500100112
012600100113       mon_file= 'A'  ;
012700100113       mon_rmn=i_rmn  ;
012800100113       mon_descr=i_inutile   ;
012900100113       mon_dsped=i_tipospe  ;
013000100113       mon_dsp  =i_datspe    ;
013100100113       mon_cad  =i_cad9+i_cad5;
013200100113       mon_cli  =i_numcli       ;
013300100113       mon_colli=i_colli     ;
013400100113       mon_lod  =i_lod       ;
013500100113       mon_prd  =i_prd       ;
013600100112       write wfmond00    ;
013700100112       endif   ;
013800100113
013900100113       i_colli =%xlate(' ':'0':i_colli);
014000100113       totcol= %int(i_colli)  ;
014100100112
014200100111       ENDSR   ;
014300100111
014400071116      /end-free
014500100112     OQSYSPRT   E            errore      1
014600100112     O                                              'I colli non tornano!! RMN'
014700100112     O                       vabrmn           +   1
014800100112     O                                        +   2 'Cad'
014900100112     O                       vabCAD           +   1
015000100112     O                                        +   2 'Lod'
015100100112     O                       vablod           +   1
015200100112     O                                        +   2 'Prd'
015300100112     O                       vabprd           +   1
015400100112     OQSYSPRT   E            errore      1
015500100112     O                                        +  30 'Ncl'
015600100112     O                       vabncl        2  +   1
015700100112     O                                              'Colli da cli'
015800100112     O                       totcol        2  +   1
