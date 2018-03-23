000100031205      *------------------------------------------------------------------------*
000200100108      * Tassazione bolle mondoffice
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600120928      *  File di work  con ovrdbf
000700120928      *  OVRDBF FILE(WFTAKSMF) TOFILE(WFTAK00F)
000800120928      *------------------------------------------------------------------------*
000900120927     ftabel00f  if a e           k Disk
001000120927     ffivab01l  if a e           k Disk
001100120926     fwftakSMf  if a e           k Disk
001200130604     Fsiem_13   iF   E             DISK
001300100112     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400031205
001500080103     d
001600120927     d privolta        s              1  0
001700120927     d wdue            s              2
001800120927     d numdec          s              2  0
001900120927     d diff            s              3  0
002000120927     d pos             s              3  0
002100120927     d pos1            s              3  0
002200120927     d pos2            s              3  0
002300120927     d wprd            s              2
002400120927     d kprd            s              2
002500130604     d savlod          s                   like(lod)
002600120927     d savprd          s                   like(wprd)
002700120927     d savcad          s                   like(vabcad)
002800130604     d wlod            s                   like(lod)
002900120927     d conta           s             15  0
003000130604     d RECORD        E DS                  EXTNAME(SIEM_13)
003100120927     d
003200120927     d TISI95DS      E ds
003300100111     D CMAIUS          C                   CONST('ABCDEFGHIJKLMNOPQRS-
003400100111     D                                     TUVWYXZ')
003500100111     D CMINUS          C                   CONST('abcdefghijklmnopqrs-
003600100111     D                                     tuvwyxz')
003700120927     d decpos          c                   %decpos(numdec)
003800120927     d
003900120928      /copy gaitrasrc/srcprotopr,tisi95r
004000071122      *------------------------------------------------------------------------*
004100071116      /free
004200071122       *inlr = *on   ;
004300130604       conta=1  ;
004400071122
004500120928       exec sql   delete from wftaksmf  where tak_rmn < 500000  ;
004600120928
004700130604       exec sql   DECLARE a1 cursor FOR  select * from siem_13
004800130604                  order by cad, lod;
004900120927
005000120927       exec sql OPEN a1   ;
005100120927
005200120927       clear sqlcod   ;
005300120927
005400120927       dow sqlcod=0   ;
005500120927
005600130604       exec sql FETCH NEXT  FROM a1 INTO :RECORD ;
005700080103
005800120927         select ;
005900120927         when sqlcod=100 ;
006000120927
006100120927         when sqlcod<0   ;
006200120927         *inh1=*on   ;
006300120927
006400120927         other   ;
006500100111         exsr elabora   ;
006600120927       endsl    ;
006700120927
006800120927         enddo   ;
006900120927
007000120927         exec sql CLOSE a1   ;
007100080107
007200100111       //------------------------------------------/
007300100111       BEGSR ELABORA   ;
007400100111
007500130604       // Elaboro la prima volta
007600130604       clear fivab000   ;
007700130604       vabccm=0435730   ;
007800120927       vabfgs=043       ;
007900120927       vablnp=043       ;
008000130604       vabaas=2013      ;
008100100112       vabctm='6 '      ;
008200100111       vabcbo='1 '      ;
008300130604       vabrsd=RSD                ;
008400130604       vabIND='feb-apr 2013  '   ;
008500130604       cad =%xlate(' ':'0':cAD) ;
008600130604       vabCAD=CAD      ;
008700120927       vablod  =%xlate(cminus:cmaius:vablod) ;
008800100111       vabnas='FATTURAZ' ;
008900130604       vabncl=ncl        ;
009000100111
009100120927       // progressivo per il riferimento mitt
009200130604       conta   = conta + 1  ;
009300130604       vabrmn  = conta                 ;
009400130604       vabrma=dsp         ;
009500130604       eval(H) vabpkb=pkb     ;
009600130604
009700130604       if vlb >99,99  ;
009800130604        vabvlb=99,99  ;
009900130604       else  ;
010000130604        eval(H) vabvlb=vlb     ;
010100130604       endif  ;
010200120927
010300120927       exsr   norm_LOD  ;
010400120927
010500120927       vabLOD=wLOD      ;
010600120927       vabprd=wprd       ;
010700120927
010800120927       // mi salvo la versione
010900130604       savlod=lod  ;
011000120927       savprd=wprd ;
011100120927       savcad=vabcad  ;
011200130604
011300130604
011400130604         write fivab000  ;
011500100112
011600120927       clear wftak000   ;
011700100112
011800130604       tak_rec = dsp+' ' +%editc(ncl:'Z') + ' ' +%editc(pkb:'Z')+' ' +
011900130604                         %editc(vlb:'Z')+' ' +cad+ ' ' +lod + ' ' +rsd  ;
012000120927       tak_rmn =vabrmn ;
012100120928         write wftak000    ;
012200100112
012300100111       ENDSR   ;
012400100111
012500120927       //----------------------
012600120927       BEGSR Norm_lod  ;
012700120927       // Se cap e località = ai precedenti, non faccio nulla e già imposto
012800130604       if savlod=lod and savcad=vabcad ;
012900120927        wprd=savprd  ;
013000120927       else  ;
013100120927
013200120927       clear wprd ;
013300120927
013400130604       wlod= lod  ;
013500120927
013600120927       // converto minuscole in maiuscole
013700120927       wlod=%xlate(cminus:cmaius:wlod)  ;
013800130605       // tolgo blanks davanti
013900130605       wlod=%trim(wlod)  ;
014000120927
014100120927       // se trovo '?'   sostituisco solo con '
014200120927       wdue='?'' ''' ;
014300120927       Pos=%scan(wdue :wlod)  ;
014400120927       if pos>0 ;
014500120927       wlod=%replace('''':wlod :pos: 2)  ;
014600120927       endif  ;
014700120927
014800120927       // tolgo la parentesi e salvo la provincia se 2 caratteri dentro
014900120927       Pos1=%scan('(' :wlod)  ;
015000120927       Pos2=%scan(')' :wlod)  ;
015100120927
015200120927       if pos1>0 and pos2>0     ;
015300120927       diff=pos2-pos1           ;
015400120927       if diff= 3   ;
015500120927       wprd=%subst(wlod:pos1+1:2)  ;
015600120927       wlod=%replace(' ':wlod :pos1: 4)  ;
015700120927       endif  ;
015800120927       endif  ;
015900120927
016000130604       if  %subst(cad:1:2)='00' ;
016100120927       if wprd='  '  ;
016200120927        wprd='RM'  ;
016300120927       endif   ;
016400120927       endif  ;
016500120927
016600120927       Pos=%scan('SANTA PALOMBA':wlod)  ;
016700120927       if POS>0  ;
016800120927        WLOD='SANTA PALOMBA            '    ;
016900120927       ENDIF  ;
017000120928       Pos=%scan('SANTA RUFINA':wlod)  ;
017100130604       if POS>0  AND CAD='02010';
017200120928        WLOD='SANTA RUFINA             '    ;
017300120928        WPRD='RI'     ;
017400120928       ENDIF  ;
017500120928       Pos=%scan('B.GO SAN DO':wlod)  ;
017600130604       if POS>0  AND CAD ='04010';
017700120928        WLOD='BORGO SAN DONATO               '    ;
017800120928        WPRD='LT'     ;
017900120928       ENDIF  ;
018000120928       Pos=%scan('B.GO S. DO':wlod)  ;
018100130604       if POS>0  AND CAD ='04010';
018200120928        WLOD='BORGO SAN DONATO               '    ;
018300120928        WPRD='LT'     ;
018400120928       ENDIF  ;
018500120928       Pos=%scan('BORG.S.DO':wlod)  ;
018600130604       if POS>0  AND CAD ='04010';
018700120928        WLOD='BORGO SAN DONATO               '    ;
018800120928        WPRD='LT'     ;
018900120928       ENDIF  ;
019000120928
019100120928       Pos=%scan('(TORINO)':wlod)  ;
019200120928       if POS>1  ;
019300120928       wlod=%replace(' ':wlod :pos: 10) ;
019400120928       endif  ;
019500120928       Pos=%scan('(CUNEO)':wlod)  ;
019600120928       if POS>1  ;
019700120928       wlod=%replace(' ':wlod :pos: 7) ;
019800120928       endif  ;
019900120927
020000120927
020100120927       // lettura provincie italia
020200120927       setll (1:'PR') tabel  ;
020300120927       reade (1:'PR') tabel  ;
020400120927       dow not %eof(tabel00f)  ;
020500120927       if tblflg=' ' ;
020600120927       if %subst(tbluni: 23: 1)='I'  ;
020700120927       kprd=%subst(tblkey:1:2)  ;
020800120927       EXSR CERCA_PRO     ;
020900120927       endif  ;
021000120927       endif  ;
021100120927
021200120927       reade (1:'PR') tabel  ;
021300120927       enddo  ;
021400120927
021500120927       if wprd='  '   ;
021600120927       clear tisi95ds  ;
021700120927       i95cap=vabcad  ;
021800120927       i95tcn='4'  ;
021900120927       i95loc=wlod  ;
022000120928       ChkCapLocalita     (tisi95ds)  ;
022100120928       if o95err=' ' and o95lid>='3' and o95lia>'0'  ;
022200120928       wprd=o95prv ;
022300120928       endif  ;
022400120928       endif  ;
022500120927
022600120927       except loc   ;
022700120927       endif  ;
022800120927
022900120927       ENDSR  ;
023000120927       //--------------------------------------
023100120927       BEGSR  Cerca_PRO   ;
023200120927
023300120927       Pos=%scan('- '+kprd
023400120927                     +' ' :WLod)  ;
023500120927       if pos>0   ;
023600120927       wprd=kprd                   ;
023700120927       wlod=%replace(' ':wlod :pos: 10) ;
023800120927       endif  ;
023900120927
024000120927       Pos=%scan('-'+kprd
024100120927                    +' '  :WLod)  ;
024200120927       if pos>0   ;
024300120927       wprd=kprd                   ;
024400120927       wlod=%replace(' ':wlod :pos: 10) ;
024500120927       endif  ;
024600120927
024700120927       Pos=%scan('-'+kprd
024800120927                    +'-'  :WLod)  ;
024900120927       if pos>0   ;
025000120927       wprd=kprd                   ;
025100120927       wlod=%replace(' ':wlod :pos: 10) ;
025200120927       endif  ;
025300120927
025400120927       Pos=%scan('( '+kprd+' )':WLod)  ;
025500120927       if pos>0   ;
025600120927       wprd=kprd                   ;
025700120927       wlod=%replace(' ':wlod :pos: 10) ;
025800120927       endif  ;
025900120927
026000120927       Pos=%scan('( '+kprd+')':WLod)  ;
026100120927       if pos>0   ;
026200120927       wprd=kprd                   ;
026300120927       wlod=%replace(' ':wlod :pos: 10) ;
026400120927       endif  ;
026500120927
026600120927       Pos=%scan('/ '+kprd+')':WLod)  ;
026700120927       if pos>0   ;
026800120927       wprd=kprd                   ;
026900120927       wlod=%replace(' ':wlod :pos: 10) ;
027000120927       endif  ;
027100120927
027200120927       Pos=%scan('/ '+kprd+' ':WLod)  ;
027300120927       if pos>0   ;
027400120927       wprd=kprd                   ;
027500120927       wlod=%replace(' ':wlod :pos: 10) ;
027600120927       endif  ;
027700120927
027800120927       Pos=%scan(', '+kprd+' ':WLod)  ;
027900120927       if pos>0   ;
028000120927       wprd=kprd                   ;
028100120927       wlod=%replace(' ':wlod :pos: 10) ;
028200120927       endif  ;
028300120927
028400120927       Pos=%scan(',  '+kprd+' ':WLod)  ;
028500120927       if pos>0   ;
028600120927       wprd=kprd                   ;
028700120927       wlod=%replace(' ':wlod :pos: 10) ;
028800120927       endif  ;
028900120927
029000120927       Pos=%scan(',  '+kprd+')':WLod)  ;
029100120927       if pos>0   ;
029200120927       wprd=kprd                   ;
029300120927       wlod=%replace(' ':wlod :pos: 10) ;
029400120927       endif  ;
029500120927
029600120927       Pos=%scan(' '+kprd+' ':wlod)  ;
029700120927       if pos>0   ;
029800120927       wprd=kprd                   ;
029900120927       wlod=%replace(' ':wlod :pos: 10) ;
030000120927       endif  ;
030100120927
030200120927       ENDSR  ;
030300120927
030400071116      /end-free
030500120927     OQSYSPRT   E            loc         1
030600120927     O                                              'localita :'
030700130604     O                       cad              +   1
030800130604     O                       LOD              +   1
030900120927     O                       wlod             +   1
031000120927     o                                        +   1 'prov:'
031100120927     O                       wprd             +   1
