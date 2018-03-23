000100031205      *------------------------------------------------------------------------*
000200120928      * Tassazione bolle Siemens  < 32 kg
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600120928      *  File di work  con ovrdbf
000700120928      *  OVRDBF FILE(WFTAKSMF) TOFILE(WFTAK00F)
000800120928      *------------------------------------------------------------------------*
000900120927     ftabel00f  if a e           k Disk
001000120927     ffivab01l  if a e           k Disk
001100120926     fwftakSMf  if a e           k Disk
001200130606     Fsiem_may  iF   E             DISK
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
002300130606     d bolle           s              5  0
002400120927     d wprd            s              2
002500120927     d kprd            s              2
002600130606     d savlod          s                   like(lod)
002700120927     d savprd          s                   like(wprd)
002800120927     d savcad          s                   like(vabcad)
002900130606     d wlod            s                   like(lod)
003000130606     d wpeso           s                   like(pkb)
003100130606     d wvolume         s                   like(vlb)
003200130606     d savrsd          s                   like(rsd)
003300130606     d savspe          s                   like(dsp)
003400120927     d totpes          s             15  5
003500120927     d totvol          s             15  5
003600100112     d totcol          s             15  0
003700120927     d conta           s             15  0
003800120927     d
003900120927     d TISI95DS      E ds
004000100111     D CMAIUS          C                   CONST('ABCDEFGHIJKLMNOPQRS-
004100100111     D                                     TUVWYXZ')
004200100111     D CMINUS          C                   CONST('abcdefghijklmnopqrs-
004300100111     D                                     tuvwyxz')
004400120927     d decpos          c                   %decpos(numdec)
004500130606     d RECORD        E DS                  EXTNAME(SIEM_may)
004600120927     d
004700120928      /copy gaitrasrc/srcprotopr,tisi95r
004800120928
004900071122      *------------------------------------------------------------------------*
005000071116      /free
005100071122       *inlr = *on   ;
005200071122
005300120928       // pulisco il file con rmn>500.000
005400120928       exec sql   delete from wftaksmf  where tak_rmn>500000  ;
005500120928
005600130606       exec sql   DECLARE a1 cursor FOR  select * from siem_may
005700130606                  order by dsp,  rsd, lod, cad        ;
005800120927
005900120927       exec sql OPEN a1   ;
006000120927
006100120927       clear sqlcod   ;
006200120927
006300120927       dow sqlcod=0   ;
006400120927
006500130606       exec sql FETCH NEXT  FROM a1 INTO: record     ;
006600080103
006700120927         select ;
006800120927         when sqlcod=100 ;
006900120927
007000120927       // Ultima scrittura
007100120928       if totpes>0   ;
007200120927       vabncl=totcol  ;
007300120927       eval(H) vabpkb=totpes  ;
007400120928       if totvol>99,99  ;
007500130606       vabvlb=70,00  ;
007600120928       else  ;
007700120928       eval(H) vabvlb=totvol  ;
007800120928       endif  ;
007900130606       %subst(vabrma:11:5)=%editc(bolle: 'X')  ;
008000130606       clear bolle  ;
008100120927
008200120928          write fivab000    ;
008300120927       endif   ;
008400120927
008500120927         when sqlcod<0   ;
008600120927         *inh1=*on   ;
008700120927
008800120927         other   ;
008900100108
009000120927       // memorizzo ragione sociale e data a rottura scrivo fivab
009100120927       if   savrsd=*blanks  ;
009200130606         savrsd=rsd   ;
009300130606         savspe=dsp;
009400130606         conta=500001 ;
009500120927       clear Privolta  ;
009600130606       clear bolle     ;
009700100108       endif    ;
009800100108
009900130606       if   savrsd<>rsd or  savspe<>dsp      ;
010000120927       vabncl=totcol  ;
010100120927       eval(H) vabpkb=totpes  ;
010200120928       if totvol>99,99  ;
010300130606       vabvlb=70,00  ;
010400120928       else  ;
010500120928       eval(H) vabvlb=totvol  ;
010600120928       endif  ;
010700130606       %subst(vabrma:11:5)=%editc(bolle: 'X')  ;
010800130606       clear bolle  ;
010900120927
011000120928         write fivab000  ;
011100100112
011200100111       clear fivab000    ;
011300100111       clear totpes  ;
011400100111       clear totvol  ;
011500120927       clear totcol  ;
011600120927       conta = conta + 1  ;
011700120927       clear Privolta  ;
011800130606         savrsd=rsd   ;
011900130606         savspe=dsp        ;
012000100111       endif    ;
012100100111
012200130606       if   savrsd=rsd and savspe=dsp        ;
012300100111
012400130606         totpes= totpes+ pkb           ;
012500130606         totvol= totvol+ vlb           ;
012600130606         totcol= totcol+ ncl           ;
012700130606         bolle = bolle + 1             ;
012800100111
012900100111         exsr elabora   ;
013000120927       endif    ;
013100120927       endsl    ;
013200120927
013300120927         enddo   ;
013400120927
013500120927         exec sql CLOSE a1   ;
013600080107
013700100111       //------------------------------------------/
013800100111       BEGSR ELABORA   ;
013900100111
014000120927       // elaboro la prima volta
014100120927       if Privolta=0   ;
014200120927
014300130606       vabccm=0435730   ;
014400120927       vabfgs=043       ;
014500120927       vablnp=043       ;
014600120927       vabaas=2012      ;
014700100112       vabctm='6 '      ;
014800100111       vabcbo='1 '      ;
014900130606       vabrsd=rsd                  ;
015000130606       vabIND='maggio 2012   '   ;
015100130606       vabCAD=cad        ;
015200100111       vabnas='FATTURAZ' ;
015300100111
015400120927       // progressivo per il riferimento mitt
015500120927       vabrmn  =conta                 ;
015600130606       vabrma=DSP             ;
015700120927
015800120927       exsr   norm_LOD  ;
015900120927
016000120927       vabLOD=wLOD      ;
016100120927       vabprd=wprd       ;
016200120927
016300120927       // mi salvo la versione
016400130606       savlod=lod  ;
016500120927       savprd=wprd ;
016600120927       savcad=vabcad  ;
016700120927
016800120927       Privolta=1  ;
016900120927       endif  ;
017000100112
017100120927       clear wftak000   ;
017200100112
017300130606       tak_rec = dsp+' ' +%editc(ncl:'Z') + ' ' +%editc(pkb:'Z')+' ' +
017400130606                         %editc(vlb:'Z')+' ' +cad+ ' ' +lod + ' ' +rsd  ;
017500120927       tak_rmn =vabrmn ;
017600120928         write wftak000    ;
017700100112
017800100111       ENDSR   ;
017900100111
018000120927       //----------------------
018100120927       BEGSR Norm_lod  ;
018200120927       // Se cap e località = ai precedenti, non faccio nulla e già imposto
018300130606       if savlod=  lod and savcad=vabcad ;
018400120927        wprd=savprd  ;
018500120927       else  ;
018600120927
018700120927       clear wprd ;
018800120927
018900130606       wlod= lod  ;
019000120927
019100120927       // converto minuscole in maiuscole
019200120927       wlod=%xlate(cminus:cmaius:wlod)  ;
019300120928
019400120927
019500120927       // se trovo '?'   sostituisco solo con '
019600120927       wdue='?'' ''' ;
019700120927       Pos=%scan(wdue :wlod)  ;
019800120927       if pos>0 ;
019900120927       wlod=%replace('''':wlod :pos: 2)  ;
020000120927       endif  ;
020100120927
020200120927       // tolgo la parentesi e salvo la provincia se 2 caratteri dentro
020300120927       Pos1=%scan('(' :wlod)  ;
020400120927       Pos2=%scan(')' :wlod)  ;
020500120927
020600120927       if pos1>0 and pos2>0     ;
020700120927       diff=pos2-pos1           ;
020800120927       if diff= 3   ;
020900120927       wprd=%subst(wlod:pos1+1:2)  ;
021000120927       wlod=%replace(' ':wlod :pos1: 4)  ;
021100120927       endif  ;
021200120927       endif  ;
021300120927
021400130606       if  %subst(cad:1:2)='00' ;
021500120927       if wprd='  '  ;
021600120927        wprd='RM'  ;
021700120927       endif   ;
021800120927       endif  ;
021900120927
022000120927       Pos=%scan('SANTA PALOMBA':wlod)  ;
022100120927       if POS>0  ;
022200120928        WLOD='SANTA PALOMBA                 '    ;
022300120927       ENDIF  ;
022400120928       Pos=%scan('SANTA RUFINA':wlod)  ;
022500130606       if POS>0  AND CAD ='02010';
022600120928        WLOD='SANTA RUFINA                  '    ;
022700120928        WPRD='RI'     ;
022800120928       ENDIF  ;
022900120928       Pos=%scan('B.GO SAN DO':wlod)  ;
023000130606       if POS>0  AND CAD ='04010';
023100120928        WLOD='BORGO SAN DONATO               '    ;
023200120928        WPRD='LT'     ;
023300120928       ENDIF  ;
023400120928       Pos=%scan('B.GO S. DO':wlod)  ;
023500130606       if POS>0  AND CAD ='04010';
023600120928        WLOD='BORGO SAN DONATO               '    ;
023700120928        WPRD='LT'     ;
023800120928       ENDIF  ;
023900120928       Pos=%scan('BORG.S.DO':wlod)  ;
024000130606       if POS>0  AND CAD ='04010';
024100120928        WLOD='BORGO SAN DONATO               '    ;
024200120928        WPRD='LT'     ;
024300120928       ENDIF  ;
024400120928
024500120928
024600120928       Pos=%scan('(TORINO)':wlod)  ;
024700120928       if POS>1  ;
024800120928       wlod=%replace(' ':wlod :pos: 10) ;
024900120928       endif  ;
025000120928       Pos=%scan('(CUNEO)':wlod)  ;
025100120928       if POS>1  ;
025200120928       wlod=%replace(' ':wlod :pos: 7) ;
025300120928       endif  ;
025400120927
025500120927
025600120927       // lettura provincie italia
025700120927       setll (1:'PR') tabel  ;
025800120927       reade (1:'PR') tabel  ;
025900120927       dow not %eof(tabel00f)  ;
026000120927       if tblflg=' ' ;
026100120927       if %subst(tbluni: 23: 1)='I'  ;
026200120927       kprd=%subst(tblkey:1:2)  ;
026300120927       EXSR CERCA_PRO     ;
026400120927       endif  ;
026500120927       endif  ;
026600120927
026700120927       reade (1:'PR') tabel  ;
026800120927       enddo  ;
026900120927
027000120927       kprd='TO'   ;
027100120927       kprd='PG'   ;
027200120927       kprd='FR'   ;
027300120927       kprd='CN'   ;
027400120927       kprd='MI'   ;
027500120927       kprd='GE'   ;
027600120927       kprd='MB'   ;
027700120927       kprd='VA'   ;
027800120927       kprd='CO'   ;
027900120927       kprd='BG'   ;
028000120927       kprd='BS'   ;
028100120927       kprd='CR'   ;
028200120927
028300120927       if wprd='  '   ;
028400120927       clear tisi95ds  ;
028500120927       i95cap=vabcad  ;
028600120927       i95tcn='4'  ;
028700120927       i95loc=wlod  ;
028800120928       ChkCapLocalita     (tisi95ds)  ;
028900120928       if o95err=' ' and o95lid>='3' and o95lia>'0'  ;
029000120928       wprd=o95prv ;
029100130606       else  ;
029200130606       i95tcn='3'  ;
029300130606       ChkCapLocalita     (tisi95ds)  ;
029400130606       if o95err=' ' and o95lid>='2'    ;
029500130606       wprd=o95prv ;
029600130606       endif  ;
029700130606       endif  ;
029800130606        endif  ;
029900120927
030000120927       except loc   ;
030100120927       endif  ;
030200120927
030300120927       ENDSR  ;
030400120927       //--------------------------------------
030500120927       BEGSR  Cerca_PRO   ;
030600120927
030700120927       Pos=%scan('- '+kprd
030800120927                     +' ' :WLod)  ;
030900120927       if pos>0   ;
031000120927       wprd=kprd                   ;
031100120927       wlod=%replace(' ':wlod :pos: 10) ;
031200120927       endif  ;
031300120927
031400120927       Pos=%scan('-'+kprd
031500120927                    +' '  :WLod)  ;
031600120927       if pos>0   ;
031700120927       wprd=kprd                   ;
031800120927       wlod=%replace(' ':wlod :pos: 10) ;
031900120927       endif  ;
032000120927
032100120927       Pos=%scan('-'+kprd
032200120927                    +'-'  :WLod)  ;
032300120927       if pos>0   ;
032400120927       wprd=kprd                   ;
032500120927       wlod=%replace(' ':wlod :pos: 10) ;
032600120927       endif  ;
032700120927
032800120927       Pos=%scan('( '+kprd+' )':WLod)  ;
032900120927       if pos>0   ;
033000120927       wprd=kprd                   ;
033100120927       wlod=%replace(' ':wlod :pos: 10) ;
033200120927       endif  ;
033300120927
033400120927       Pos=%scan('( '+kprd+')':WLod)  ;
033500120927       if pos>0   ;
033600120927       wprd=kprd                   ;
033700120927       wlod=%replace(' ':wlod :pos: 10) ;
033800120927       endif  ;
033900120927
034000120927       Pos=%scan('/ '+kprd+')':WLod)  ;
034100120927       if pos>0   ;
034200120927       wprd=kprd                   ;
034300120927       wlod=%replace(' ':wlod :pos: 10) ;
034400120927       endif  ;
034500120927
034600120927       Pos=%scan('/ '+kprd+' ':WLod)  ;
034700120927       if pos>0   ;
034800120927       wprd=kprd                   ;
034900120927       wlod=%replace(' ':wlod :pos: 10) ;
035000120927       endif  ;
035100120927
035200120927       Pos=%scan(', '+kprd+' ':WLod)  ;
035300120927       if pos>0   ;
035400120927       wprd=kprd                   ;
035500120927       wlod=%replace(' ':wlod :pos: 10) ;
035600120927       endif  ;
035700120927
035800120927       Pos=%scan(',  '+kprd+' ':WLod)  ;
035900120927       if pos>0   ;
036000120927       wprd=kprd                   ;
036100120927       wlod=%replace(' ':wlod :pos: 10) ;
036200120927       endif  ;
036300120927
036400120927       Pos=%scan(',  '+kprd+')':WLod)  ;
036500120927       if pos>0   ;
036600120927       wprd=kprd                   ;
036700120927       wlod=%replace(' ':wlod :pos: 10) ;
036800120927       endif  ;
036900120927
037000120927       Pos=%scan(' '+kprd+' ':wlod)  ;
037100120927       if pos>0   ;
037200120927       wprd=kprd                   ;
037300120927       wlod=%replace(' ':wlod :pos: 10) ;
037400120927       endif  ;
037500120927
037600120927       ENDSR  ;
037700120927
037800071116      /end-free
037900120927     OQSYSPRT   E            loc         1
038000120927     O                                              'localita :'
038100130606     O                       cad              +   1
038200130606     O                       lod              +   1
038300120927     O                       wlod             +   1
038400120927     o                                        +   1 'prov:'
038500120927     O                       wprd             +   1
