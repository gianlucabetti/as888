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
001200120926     Fsimens_alliF   E             DISK    rename(simens_all:simens000)
001300100112     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400031205
001500120926     D DSrec           DS
001600120926     d  I_inutile              1     50
001700120926     d  I_datspe              51     60
001800120926     d  I_colli               61     67
001900120926     d  I_peso                68     76
002000120926     d  I_volume              77     83
002100120926     d  I_nzd                 84     85
002200120926     d  I_cad9                86     95
002300120926     d  I_cad5                91     95
002400120926     d  I_vuoto               96     99
002500120926     d  I_lod                100    140
002600120926     d  I_rsd                141    170
002700120927     d  I_inutile2           171    220
002800080103     d
002900120927     d privolta        s              1  0
003000120927     d wdue            s              2
003100120927     d numdec          s              2  0
003200120927     d diff            s              3  0
003300120927     d pos             s              3  0
003400120927     d pos1            s              3  0
003500120927     d pos2            s              3  0
003600120927     d wprd            s              2
003700120927     d kprd            s              2
003800120927     d savlod          s                   like(i_lod)
003900120927     d savprd          s                   like(wprd)
004000120927     d savcad          s                   like(vabcad)
004100120927     d wlod            s                   like(i_lod)
004200120927     d wpeso           s                   like(i_peso)
004300120927     d wvolume         s                   like(i_volume)
004400120927     d wcolli          s                   like(i_colli)
004500120927     d savrsd          s                   like(i_rsd)
004600120927     d savspe          s                   like(i_datspe)
004700120927     d totpes          s             15  5
004800120927     d totvol          s             15  5
004900100112     d totcol          s             15  0
005000120927     d conta           s             15  0
005100120927     d
005200120927     d TISI95DS      E ds
005300100111     D CMAIUS          C                   CONST('ABCDEFGHIJKLMNOPQRS-
005400100111     D                                     TUVWYXZ')
005500100111     D CMINUS          C                   CONST('abcdefghijklmnopqrs-
005600100111     D                                     tuvwyxz')
005700120927     d decpos          c                   %decpos(numdec)
005800120927     d
005900120928      /copy gaitrasrc/srcprotopr,tisi95r
006000120928
006100120927     isimens000
006200120927     I              SIMENS_ALL                  simens_fld
006300071122      *------------------------------------------------------------------------*
006400071116      /free
006500071122       *inlr = *on   ;
006600071122
006700120928       exec sql   delete from wftaksmf  where tak_rmn < 500000  ;
006800120928
006900120928       exec sql   DECLARE a1 cursor FOR  select * from simens_all
007000120927                  where substr(simens_all, 51, 10)<>'          '
007100120927                  and substr(simens_all, 1, 8)<>'Delivery'
007200120927                  order by substr(simens_all, 141, 30),
007300120927                           substr(simens_all, 51, 10)        ;
007400120927
007500120927       exec sql OPEN a1   ;
007600120927
007700120927       clear sqlcod   ;
007800120927
007900120927       dow sqlcod=0   ;
008000120927
008100120927       exec sql FETCH NEXT  FROM a1 INTO: simens_fld ;
008200080103
008300120927         select ;
008400120927         when sqlcod=100 ;
008500120927
008600120927       // Ultima scrittura
008700120928       if totpes>0   ;
008800120927       vabncl=totcol  ;
008900120927       eval(H) vabpkb=totpes  ;
009000120928       if totvol>99,99  ;
009100120928       vabvlb=99,99  ;
009200120928       else  ;
009300120928       eval(H) vabvlb=totvol  ;
009400120928       endif  ;
009500120927
009600120928          write fivab000    ;
009700120927       endif   ;
009800120927
009900120927         when sqlcod<0   ;
010000120927         *inh1=*on   ;
010100120927
010200120927         other   ;
010300120927       dsrec=simens_fld    ;
010400100108
010500120927       // memorizzo ragione sociale e data a rottura scrivo fivab
010600120927       if   savrsd=*blanks  ;
010700120927         savrsd=i_rsd   ;
010800120927         savspe=i_datspe;
010900120927         conta=1  ;
011000120927       clear Privolta  ;
011100100108       endif    ;
011200100108
011300120927       if   savrsd<>i_rsd or  savspe<>i_datspe   ;
011400120927       vabncl=totcol  ;
011500120927       eval(H) vabpkb=totpes  ;
011600120928       if totvol>99,99  ;
011700120928       vabvlb=99,99  ;
011800120928       else  ;
011900120928       eval(H) vabvlb=totvol  ;
012000120928       endif  ;
012100120927
012200120928         write fivab000  ;
012300100112
012400100111       clear fivab000    ;
012500100111       clear totpes  ;
012600100111       clear totvol  ;
012700120927       clear totcol  ;
012800120927       conta = conta + 1  ;
012900120927       clear Privolta  ;
013000120927         savrsd=i_rsd   ;
013100120927         savspe=i_datspe   ;
013200100111       endif    ;
013300100111
013400120927       if   savrsd=i_rsd and savspe=i_datspe   ;
013500100111
013600120927         wpeso=%xlate(' ':'0':i_peso) ;
013700120927         Pos1=%scan(','  :wpeso)  ;
013800120927         if pos1=0  ;
013900120927          totpes= totpes+ %int(wpeso)  ;
014000120927         else  ;
014100120927         numdec=9-pos1  ;
014200120927          select  ;
014300120927          when numdec=1  ;
014400120927          totpes= totpes+ %dec(wpeso:9:1)  ;
014500120927          when numdec=2  ;
014600120927          totpes= totpes+ %dec(wpeso:9:2)  ;
014700120927          when numdec=3  ;
014800120927          totpes= totpes+ %dec(wpeso:9:3)  ;
014900120927          when numdec=4  ;
015000120927          totpes= totpes+ %dec(wpeso:9:4)  ;
015100120927          when numdec=5  ;
015200120927          totpes= totpes+ %dec(wpeso:9:5)  ;
015300120927          endsl  ;
015400120927
015500120927         endif  ;
015600100111
015700120927         wvolume=%xlate(' ':'0':i_volume) ;
015800120927         Pos1=%scan(','  :wvolume)  ;
015900120927         if pos1=0  ;
016000120927          totvol= totvol+ %int(wvolume);
016100120927         else  ;
016200120927         numdec=7-pos1  ;
016300120927          select  ;
016400120927          when numdec=1  ;
016500120927          totvol= totvol+ %dec(wvolume:7:1)  ;
016600120927          when numdec=2  ;
016700120927          totvol= totvol+ %dec(wvolume:7:2)  ;
016800120927          when numdec=3  ;
016900120927          totvol= totvol+ %dec(wvolume:7:3)  ;
017000120927          when numdec=4  ;
017100120927          totvol= totvol+ %dec(wvolume:7:4)  ;
017200120927          when numdec=5  ;
017300120927          totvol= totvol+ %dec(wvolume:7:5)  ;
017400120927          endsl  ;
017500120927
017600120927         endif  ;
017700120927
017800120927         wcolli=%xlate(' ':'0':i_colli);
017900120927         totcol= totcol+ %int(wcolli)  ;
018000100111
018100100111         exsr elabora   ;
018200120927       endif    ;
018300120927       endsl    ;
018400120927
018500120927         enddo   ;
018600120927
018700120927         exec sql CLOSE a1   ;
018800080107
018900100111       //------------------------------------------/
019000100111       BEGSR ELABORA   ;
019100100111
019200120927       // elaboro la prima volta
019300120927       if Privolta=0   ;
019400120927
019500120927       vabccm=0435730   ;
019600120927       vabfgs=043       ;
019700120927       vablnp=043       ;
019800120927       vabaas=2012      ;
019900100112       vabctm='6 '      ;
020000100111       vabcbo='1 '      ;
020100120927       vabrsd=I_RSD                ;
020200120927       vabIND='LUGLIO 2012   '   ;
020300100112       i_cad5=%xlate(' ':'0':i_cAD5) ;
020400100112       vabCAD=I_CAD5     ;
020500120927       vablod  =%xlate(cminus:cmaius:vablod) ;
020600100111       vabnas='FATTURAZ' ;
020700100111       vabncl=vabncl +1  ;
020800100111
020900120927       // progressivo per il riferimento mitt
021000120927       vabrmn  =conta                 ;
021100120927       vabrma='07/12-ALL ';
021200120927
021300120927       exsr   norm_LOD  ;
021400120927
021500120927       vabLOD=wLOD      ;
021600120927       vabprd=wprd       ;
021700120927
021800120927       // mi salvo la versione
021900120927       savlod=I_lod  ;
022000120927       savprd=wprd ;
022100120927       savcad=vabcad  ;
022200120927
022300120927       Privolta=1  ;
022400120927       endif  ;
022500100112
022600120927       clear wftak000   ;
022700100112
022800120927       tak_rec = simens_fld  ;
022900120927       tak_rmn =vabrmn ;
023000120928         write wftak000    ;
023100100112
023200100111       ENDSR   ;
023300100111
023400120927       //----------------------
023500120927       BEGSR Norm_lod  ;
023600120927       // Se cap e località = ai precedenti, non faccio nulla e già imposto
023700120927       if savlod=I_lod and savcad=vabcad ;
023800120927        wprd=savprd  ;
023900120927       else  ;
024000120927
024100120927       clear wprd ;
024200120927
024300120927       wlod= i_lod  ;
024400120927
024500120927       // converto minuscole in maiuscole
024600120927       wlod=%xlate(cminus:cmaius:wlod)  ;
024700120928
024800120927
024900120927       // se trovo '?'   sostituisco solo con '
025000120927       wdue='?'' ''' ;
025100120927       Pos=%scan(wdue :wlod)  ;
025200120927       if pos>0 ;
025300120927       wlod=%replace('''':wlod :pos: 2)  ;
025400120927       endif  ;
025500120927
025600120927       // tolgo la parentesi e salvo la provincia se 2 caratteri dentro
025700120927       Pos1=%scan('(' :wlod)  ;
025800120927       Pos2=%scan(')' :wlod)  ;
025900120927
026000120927       if pos1>0 and pos2>0     ;
026100120927       diff=pos2-pos1           ;
026200120927       if diff= 3   ;
026300120927       wprd=%subst(wlod:pos1+1:2)  ;
026400120927       wlod=%replace(' ':wlod :pos1: 4)  ;
026500120927       endif  ;
026600120927       endif  ;
026700120927
026800120927       if  %subst(I_cad5:1:2)='00' ;
026900120927       if wprd='  '  ;
027000120927        wprd='RM'  ;
027100120927       endif   ;
027200120927       endif  ;
027300120927
027400120927       Pos=%scan('SANTA PALOMBA':wlod)  ;
027500120927       if POS>0  ;
027600120927        WLOD='SANTA PALOMBA            '    ;
027700120927       ENDIF  ;
027800120928       Pos=%scan('SANTA RUFINA':wlod)  ;
027900120928       if POS>0  AND I_CAD5='02010';
028000120928        WLOD='SANTA RUFINA             '    ;
028100120928        WPRD='RI'     ;
028200120928       ENDIF  ;
028300120928       Pos=%scan('B.GO SAN DO':wlod)  ;
028400120928       if POS>0  AND I_CAD5='04010';
028500120928        WLOD='BORGO SAN DONATO               '    ;
028600120928        WPRD='LT'     ;
028700120928       ENDIF  ;
028800120928       Pos=%scan('B.GO S. DO':wlod)  ;
028900120928       if POS>0  AND I_CAD5='04010';
029000120928        WLOD='BORGO SAN DONATO               '    ;
029100120928        WPRD='LT'     ;
029200120928       ENDIF  ;
029300120928       Pos=%scan('BORG.S.DO':wlod)  ;
029400120928       if POS>0  AND I_CAD5='04010';
029500120928        WLOD='BORGO SAN DONATO               '    ;
029600120928        WPRD='LT'     ;
029700120928       ENDIF  ;
029800120928
029900120928       Pos=%scan('(TORINO)':wlod)  ;
030000120928       if POS>1  ;
030100120928       wlod=%replace(' ':wlod :pos: 10) ;
030200120928       endif  ;
030300120928       Pos=%scan('(CUNEO)':wlod)  ;
030400120928       if POS>1  ;
030500120928       wlod=%replace(' ':wlod :pos: 7) ;
030600120928       endif  ;
030700120927
030800120927
030900120927       // lettura provincie italia
031000120927       setll (1:'PR') tabel  ;
031100120927       reade (1:'PR') tabel  ;
031200120927       dow not %eof(tabel00f)  ;
031300120927       if tblflg=' ' ;
031400120927       if %subst(tbluni: 23: 1)='I'  ;
031500120927       kprd=%subst(tblkey:1:2)  ;
031600120927       EXSR CERCA_PRO     ;
031700120927       endif  ;
031800120927       endif  ;
031900120927
032000120927       reade (1:'PR') tabel  ;
032100120927       enddo  ;
032200120927
032300120927       if wprd='  '   ;
032400120927       clear tisi95ds  ;
032500120927       i95cap=vabcad  ;
032600120927       i95tcn='4'  ;
032700120927       i95loc=wlod  ;
032800120928       ChkCapLocalita     (tisi95ds)  ;
032900120928       if o95err=' ' and o95lid>='3' and o95lia>'0'  ;
033000120928       wprd=o95prv ;
033100120928       endif  ;
033200120928       endif  ;
033300120927
033400120927       except loc   ;
033500120927       endif  ;
033600120927
033700120927       ENDSR  ;
033800120927       //--------------------------------------
033900120927       BEGSR  Cerca_PRO   ;
034000120927
034100120927       Pos=%scan('- '+kprd
034200120927                     +' ' :WLod)  ;
034300120927       if pos>0   ;
034400120927       wprd=kprd                   ;
034500120927       wlod=%replace(' ':wlod :pos: 10) ;
034600120927       endif  ;
034700120927
034800120927       Pos=%scan('-'+kprd
034900120927                    +' '  :WLod)  ;
035000120927       if pos>0   ;
035100120927       wprd=kprd                   ;
035200120927       wlod=%replace(' ':wlod :pos: 10) ;
035300120927       endif  ;
035400120927
035500120927       Pos=%scan('-'+kprd
035600120927                    +'-'  :WLod)  ;
035700120927       if pos>0   ;
035800120927       wprd=kprd                   ;
035900120927       wlod=%replace(' ':wlod :pos: 10) ;
036000120927       endif  ;
036100120927
036200120927       Pos=%scan('( '+kprd+' )':WLod)  ;
036300120927       if pos>0   ;
036400120927       wprd=kprd                   ;
036500120927       wlod=%replace(' ':wlod :pos: 10) ;
036600120927       endif  ;
036700120927
036800120927       Pos=%scan('( '+kprd+')':WLod)  ;
036900120927       if pos>0   ;
037000120927       wprd=kprd                   ;
037100120927       wlod=%replace(' ':wlod :pos: 10) ;
037200120927       endif  ;
037300120927
037400120927       Pos=%scan('/ '+kprd+')':WLod)  ;
037500120927       if pos>0   ;
037600120927       wprd=kprd                   ;
037700120927       wlod=%replace(' ':wlod :pos: 10) ;
037800120927       endif  ;
037900120927
038000120927       Pos=%scan('/ '+kprd+' ':WLod)  ;
038100120927       if pos>0   ;
038200120927       wprd=kprd                   ;
038300120927       wlod=%replace(' ':wlod :pos: 10) ;
038400120927       endif  ;
038500120927
038600120927       Pos=%scan(', '+kprd+' ':WLod)  ;
038700120927       if pos>0   ;
038800120927       wprd=kprd                   ;
038900120927       wlod=%replace(' ':wlod :pos: 10) ;
039000120927       endif  ;
039100120927
039200120927       Pos=%scan(',  '+kprd+' ':WLod)  ;
039300120927       if pos>0   ;
039400120927       wprd=kprd                   ;
039500120927       wlod=%replace(' ':wlod :pos: 10) ;
039600120927       endif  ;
039700120927
039800120927       Pos=%scan(',  '+kprd+')':WLod)  ;
039900120927       if pos>0   ;
040000120927       wprd=kprd                   ;
040100120927       wlod=%replace(' ':wlod :pos: 10) ;
040200120927       endif  ;
040300120927
040400120927       Pos=%scan(' '+kprd+' ':wlod)  ;
040500120927       if pos>0   ;
040600120927       wprd=kprd                   ;
040700120927       wlod=%replace(' ':wlod :pos: 10) ;
040800120927       endif  ;
040900120927
041000120927       ENDSR  ;
041100120927
041200071116      /end-free
041300120927     OQSYSPRT   E            loc         1
041400120927     O                                              'localita :'
041500120928     O                       i_cad5           +   1
041600120928     O                       i_lod            +   1
041700120927     O                       wlod             +   1
041800120927     o                                        +   1 'prov:'
041900120927     O                       wprd             +   1
