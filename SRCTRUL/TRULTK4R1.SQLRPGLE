000100031205      *------------------------------------------------------------------------*
000200100108      * Tassazione bolle mondoffice
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600120928      *  File di work  con ovrdbf
000700120928      *  OVRDBF FILE(WFTAKSMF) TOFILE(WFTAK00F)
000800120928      *------------------------------------------------------------------------*
000900120927     ftabel00f  if a e           k Disk
001000130605     ffivab01l  uf a e           k Disk
001100100112     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001200031205
001300080103     d
001400120927     d privolta        s              1  0
001500120927     d wdue            s              2
001600120927     d numdec          s              2  0
001700120927     d diff            s              3  0
001800120927     d pos             s              3  0
001900120927     d pos1            s              3  0
002000120927     d pos2            s              3  0
002100120927     d wprd            s              2
002200120927     d kprd            s              2
002300130605     d savlod          s                   like(vablod)
002400120927     d savprd          s                   like(wprd)
002500120927     d savcad          s                   like(vabcad)
002600130605     d wlod            s                   like(vablod)
002700120927     d conta           s             15  0
002800130605     d RECORD        E DS                  EXTNAME(fivab00f)
002900120927     d
003000120927     d TISI95DS      E ds
003100100111     D CMAIUS          C                   CONST('ABCDEFGHIJKLMNOPQRS-
003200100111     D                                     TUVWYXZ')
003300100111     D CMINUS          C                   CONST('abcdefghijklmnopqrs-
003400100111     D                                     tuvwyxz')
003500120927     d decpos          c                   %decpos(numdec)
003600120927     d
003700120928      /copy gaitrasrc/srcprotopr,tisi95r
003800071122      *------------------------------------------------------------------------*
003900071116      /free
004000071122       *inlr = *on   ;
004100130605       setll  0435730    fivab01l  ;
004200130605       reade  0435730    fivab01l  ;
004300130605
004400130605       dow not %eof(fivab01l)  ;
004500130605       exsr elabora        ;
004600130605
004700130605       reade  0435730    fivab01l  ;
004800130605       enddo  ;
004900080107
005000100111       //------------------------------------------/
005100100111       BEGSR ELABORA   ;
005200120927
005300120927       exsr   norm_LOD  ;
005400120927
005500120927       vabLOD=wLOD      ;
005600120927       vabprd=wprd       ;
005700120927
005800120927       // mi salvo la versione
005900130605       savlod=vablod  ;
006000120927       savprd=wprd ;
006100120927       savcad=vabcad  ;
006200130604
006300130604
006400130605         update fivab000  ;
006500100112
006600130605        ENDSR  ;
006700130605
006800120927       //----------------------
006900120927       BEGSR Norm_lod  ;
007000120927       // Se cap e località = ai precedenti, non faccio nulla e già imposto
007100130605       if savlod=vablod and savcad=vabcad ;
007200120927        wprd=savprd  ;
007300120927       else  ;
007400120927
007500120927       clear wprd ;
007600120927
007700130605       wlod= vablod  ;
007800120927       // lettura provincie italia
007900120927       setll (1:'PR') tabel  ;
008000120927       reade (1:'PR') tabel  ;
008100120927       dow not %eof(tabel00f)  ;
008200120927       if tblflg=' ' ;
008300120927       if %subst(tbluni: 23: 1)='I'  ;
008400120927       kprd=%subst(tblkey:1:2)  ;
008500120927       EXSR CERCA_PRO     ;
008600120927       endif  ;
008700120927       endif  ;
008800120927
008900120927       reade (1:'PR') tabel  ;
009000120927       enddo  ;
009100120927
009200120927       if wprd='  '   ;
009300120927       clear tisi95ds  ;
009400120927       i95cap=vabcad  ;
009500120927       i95tcn='4'  ;
009600120927       i95loc=wlod  ;
009700120928       ChkCapLocalita     (tisi95ds)  ;
009800120928       if o95err=' ' and o95lid>='3' and o95lia>'0'  ;
009900120928       wprd=o95prv ;
010000130605       else  ;
010100130605       i95tcn='3'  ;
010200130605       ChkCapLocalita     (tisi95ds)  ;
010300130605       if o95err=' ' and o95lid>='2'    ;
010400130605       wprd=o95prv ;
010500120928       endif  ;
010600120928       endif  ;
010700120927       endif  ;
010800130605
010900130605       endif  ;
011000120927
011100120927       ENDSR  ;
011200120927       //--------------------------------------
011300120927       BEGSR  Cerca_PRO   ;
011400120927
011500120927       Pos=%scan('- '+kprd
011600120927                     +' ' :WLod)  ;
011700120927       if pos>0   ;
011800120927       wprd=kprd                   ;
011900120927       wlod=%replace(' ':wlod :pos: 10) ;
012000120927       endif  ;
012100120927
012200120927       Pos=%scan('-'+kprd
012300120927                    +' '  :WLod)  ;
012400120927       if pos>0   ;
012500120927       wprd=kprd                   ;
012600120927       wlod=%replace(' ':wlod :pos: 10) ;
012700120927       endif  ;
012800120927
012900120927       Pos=%scan('-'+kprd
013000120927                    +'-'  :WLod)  ;
013100120927       if pos>0   ;
013200120927       wprd=kprd                   ;
013300120927       wlod=%replace(' ':wlod :pos: 10) ;
013400120927       endif  ;
013500120927
013600120927       Pos=%scan('( '+kprd+' )':WLod)  ;
013700120927       if pos>0   ;
013800120927       wprd=kprd                   ;
013900120927       wlod=%replace(' ':wlod :pos: 10) ;
014000120927       endif  ;
014100120927
014200120927       Pos=%scan('( '+kprd+')':WLod)  ;
014300120927       if pos>0   ;
014400120927       wprd=kprd                   ;
014500120927       wlod=%replace(' ':wlod :pos: 10) ;
014600120927       endif  ;
014700120927
014800120927       Pos=%scan('/ '+kprd+')':WLod)  ;
014900120927       if pos>0   ;
015000120927       wprd=kprd                   ;
015100120927       wlod=%replace(' ':wlod :pos: 10) ;
015200120927       endif  ;
015300120927
015400120927       Pos=%scan('/ '+kprd+' ':WLod)  ;
015500120927       if pos>0   ;
015600120927       wprd=kprd                   ;
015700120927       wlod=%replace(' ':wlod :pos: 10) ;
015800120927       endif  ;
015900120927
016000120927       Pos=%scan(', '+kprd+' ':WLod)  ;
016100120927       if pos>0   ;
016200120927       wprd=kprd                   ;
016300120927       wlod=%replace(' ':wlod :pos: 10) ;
016400120927       endif  ;
016500120927
016600120927       Pos=%scan(',  '+kprd+' ':WLod)  ;
016700120927       if pos>0   ;
016800120927       wprd=kprd                   ;
016900120927       wlod=%replace(' ':wlod :pos: 10) ;
017000120927       endif  ;
017100120927
017200120927       Pos=%scan(',  '+kprd+')':WLod)  ;
017300120927       if pos>0   ;
017400120927       wprd=kprd                   ;
017500120927       wlod=%replace(' ':wlod :pos: 10) ;
017600120927       endif  ;
017700120927
017800120927       Pos=%scan(' '+kprd+' ':wlod)  ;
017900120927       if pos>0   ;
018000120927       wprd=kprd                   ;
018100120927       wlod=%replace(' ':wlod :pos: 10) ;
018200120927       endif  ;
018300120927
018400120927       ENDSR  ;
018500120927
018600071116      /end-free
