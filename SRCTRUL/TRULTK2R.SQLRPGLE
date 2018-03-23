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
001200120928     Fsimens_u32iF   E             DISK    rename(simens_u32:simens000)
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
006200120928     I              SIMENS_U32                  simens_fld
006300071122      *------------------------------------------------------------------------*
006400071116      /free
006500071122       *inlr = *on   ;
006600071122
006700120928       // pulisco il file con rmn>500.000
006800120928       exec sql   delete from wftaksmf  where tak_rmn>500000  ;
006900120928
007000120928       exec sql   DECLARE a1 cursor FOR  select * from simens_u32
007100120928                  where substr(simens_u32, 51, 10)<>'          '
007200120928                  and substr(simens_u32, 1, 8)<>'Delivery'
007300120928                  order by substr(simens_u32, 141, 30),
007400120928                           substr(simens_u32, 51, 10)        ;
007500120927
007600120927       exec sql OPEN a1   ;
007700120927
007800120927       clear sqlcod   ;
007900120927
008000120927       dow sqlcod=0   ;
008100120927
008200120927       exec sql FETCH NEXT  FROM a1 INTO: simens_fld ;
008300080103
008400120927         select ;
008500120927         when sqlcod=100 ;
008600120927
008700120927       // Ultima scrittura
008800120928       if totpes>0   ;
008900120927       vabncl=totcol  ;
009000120927       eval(H) vabpkb=totpes  ;
009100120928       if totvol>99,99  ;
009200120928       vabvlb=99,99  ;
009300120928       else  ;
009400120928       eval(H) vabvlb=totvol  ;
009500120928       endif  ;
009600120927
009700120928          write fivab000    ;
009800120927       endif   ;
009900120927
010000120927         when sqlcod<0   ;
010100120927         *inh1=*on   ;
010200120927
010300120927         other   ;
010400120927       dsrec=simens_fld    ;
010500100108
010600120927       // memorizzo ragione sociale e data a rottura scrivo fivab
010700120927       if   savrsd=*blanks  ;
010800120927         savrsd=i_rsd   ;
010900120927         savspe=i_datspe;
011000120928         conta=500000 ;
011100120927       clear Privolta  ;
011200100108       endif    ;
011300100108
011400120927       if   savrsd<>i_rsd or  savspe<>i_datspe   ;
011500120927       vabncl=totcol  ;
011600120927       eval(H) vabpkb=totpes  ;
011700120928       if totvol>99,99  ;
011800120928       vabvlb=99,99  ;
011900120928       else  ;
012000120928       eval(H) vabvlb=totvol  ;
012100120928       endif  ;
012200120927
012300120928         write fivab000  ;
012400100112
012500100111       clear fivab000    ;
012600100111       clear totpes  ;
012700100111       clear totvol  ;
012800120927       clear totcol  ;
012900120927       conta = conta + 1  ;
013000120927       clear Privolta  ;
013100120927         savrsd=i_rsd   ;
013200120927         savspe=i_datspe   ;
013300100111       endif    ;
013400100111
013500120927       if   savrsd=i_rsd and savspe=i_datspe   ;
013600100111
013700120927         wpeso=%xlate(' ':'0':i_peso) ;
013800120927         Pos1=%scan(','  :wpeso)  ;
013900120927         if pos1=0  ;
014000120927          totpes= totpes+ %int(wpeso)  ;
014100120927         else  ;
014200120927         numdec=9-pos1  ;
014300120927          select  ;
014400120927          when numdec=1  ;
014500120927          totpes= totpes+ %dec(wpeso:9:1)  ;
014600120927          when numdec=2  ;
014700120927          totpes= totpes+ %dec(wpeso:9:2)  ;
014800120927          when numdec=3  ;
014900120927          totpes= totpes+ %dec(wpeso:9:3)  ;
015000120927          when numdec=4  ;
015100120927          totpes= totpes+ %dec(wpeso:9:4)  ;
015200120927          when numdec=5  ;
015300120927          totpes= totpes+ %dec(wpeso:9:5)  ;
015400120927          endsl  ;
015500120927
015600120927         endif  ;
015700100111
015800120927         wvolume=%xlate(' ':'0':i_volume) ;
015900120927         Pos1=%scan(','  :wvolume)  ;
016000120927         if pos1=0  ;
016100120927          totvol= totvol+ %int(wvolume);
016200120927         else  ;
016300120927         numdec=7-pos1  ;
016400120927          select  ;
016500120927          when numdec=1  ;
016600120927          totvol= totvol+ %dec(wvolume:7:1)  ;
016700120927          when numdec=2  ;
016800120927          totvol= totvol+ %dec(wvolume:7:2)  ;
016900120927          when numdec=3  ;
017000120927          totvol= totvol+ %dec(wvolume:7:3)  ;
017100120927          when numdec=4  ;
017200120927          totvol= totvol+ %dec(wvolume:7:4)  ;
017300120927          when numdec=5  ;
017400120927          totvol= totvol+ %dec(wvolume:7:5)  ;
017500120927          endsl  ;
017600120927
017700120927         endif  ;
017800120927
017900120927         wcolli=%xlate(' ':'0':i_colli);
018000120927         totcol= totcol+ %int(wcolli)  ;
018100100111
018200100111         exsr elabora   ;
018300120927       endif    ;
018400120927       endsl    ;
018500120927
018600120927         enddo   ;
018700120927
018800120927         exec sql CLOSE a1   ;
018900080107
019000100111       //------------------------------------------/
019100100111       BEGSR ELABORA   ;
019200100111
019300120927       // elaboro la prima volta
019400120927       if Privolta=0   ;
019500120927
019600120928       vabccm=0435731   ;
019700120927       vabfgs=043       ;
019800120927       vablnp=043       ;
019900120927       vabaas=2012      ;
020000100112       vabctm='6 '      ;
020100100111       vabcbo='1 '      ;
020200120927       vabrsd=I_RSD                ;
020300120927       vabIND='LUGLIO 2012   '   ;
020400100112       i_cad5=%xlate(' ':'0':i_cAD5) ;
020500100112       vabCAD=I_CAD5     ;
020600120927       vablod  =%xlate(cminus:cmaius:vablod) ;
020700100111       vabnas='FATTURAZ' ;
020800100111       vabncl=vabncl +1  ;
020900100111
021000120927       // progressivo per il riferimento mitt
021100120927       vabrmn  =conta                 ;
021200120928       vabrma='07/12 < 32 KG ';
021300120927
021400120927       exsr   norm_LOD  ;
021500120927
021600120927       vabLOD=wLOD      ;
021700120927       vabprd=wprd       ;
021800120927
021900120927       // mi salvo la versione
022000120927       savlod=I_lod  ;
022100120927       savprd=wprd ;
022200120927       savcad=vabcad  ;
022300120927
022400120927       Privolta=1  ;
022500120927       endif  ;
022600100112
022700120927       clear wftak000   ;
022800100112
022900120927       tak_rec = simens_fld  ;
023000120927       tak_rmn =vabrmn ;
023100120928         write wftak000    ;
023200100112
023300100111       ENDSR   ;
023400100111
023500120927       //----------------------
023600120927       BEGSR Norm_lod  ;
023700120927       // Se cap e località = ai precedenti, non faccio nulla e già imposto
023800120927       if savlod=I_lod and savcad=vabcad ;
023900120927        wprd=savprd  ;
024000120927       else  ;
024100120927
024200120927       clear wprd ;
024300120927
024400120927       wlod= i_lod  ;
024500120927
024600120927       // converto minuscole in maiuscole
024700120927       wlod=%xlate(cminus:cmaius:wlod)  ;
024800120928
024900120927
025000120927       // se trovo '?'   sostituisco solo con '
025100120927       wdue='?'' ''' ;
025200120927       Pos=%scan(wdue :wlod)  ;
025300120927       if pos>0 ;
025400120927       wlod=%replace('''':wlod :pos: 2)  ;
025500120927       endif  ;
025600120927
025700120927       // tolgo la parentesi e salvo la provincia se 2 caratteri dentro
025800120927       Pos1=%scan('(' :wlod)  ;
025900120927       Pos2=%scan(')' :wlod)  ;
026000120927
026100120927       if pos1>0 and pos2>0     ;
026200120927       diff=pos2-pos1           ;
026300120927       if diff= 3   ;
026400120927       wprd=%subst(wlod:pos1+1:2)  ;
026500120927       wlod=%replace(' ':wlod :pos1: 4)  ;
026600120927       endif  ;
026700120927       endif  ;
026800120927
026900120927       if  %subst(I_cad5:1:2)='00' ;
027000120927       if wprd='  '  ;
027100120927        wprd='RM'  ;
027200120927       endif   ;
027300120927       endif  ;
027400120927
027500120927       Pos=%scan('SANTA PALOMBA':wlod)  ;
027600120927       if POS>0  ;
027700120928        WLOD='SANTA PALOMBA                 '    ;
027800120927       ENDIF  ;
027900120928       Pos=%scan('SANTA RUFINA':wlod)  ;
028000120928       if POS>0  AND I_CAD5='02010';
028100120928        WLOD='SANTA RUFINA                  '    ;
028200120928        WPRD='RI'     ;
028300120928       ENDIF  ;
028400120928       Pos=%scan('B.GO SAN DO':wlod)  ;
028500120928       if POS>0  AND I_CAD5='04010';
028600120928        WLOD='BORGO SAN DONATO               '    ;
028700120928        WPRD='LT'     ;
028800120928       ENDIF  ;
028900120928       Pos=%scan('B.GO S. DO':wlod)  ;
029000120928       if POS>0  AND I_CAD5='04010';
029100120928        WLOD='BORGO SAN DONATO               '    ;
029200120928        WPRD='LT'     ;
029300120928       ENDIF  ;
029400120928       Pos=%scan('BORG.S.DO':wlod)  ;
029500120928       if POS>0  AND I_CAD5='04010';
029600120928        WLOD='BORGO SAN DONATO               '    ;
029700120928        WPRD='LT'     ;
029800120928       ENDIF  ;
029900120928
030000120928
030100120928       Pos=%scan('(TORINO)':wlod)  ;
030200120928       if POS>1  ;
030300120928       wlod=%replace(' ':wlod :pos: 10) ;
030400120928       endif  ;
030500120928       Pos=%scan('(CUNEO)':wlod)  ;
030600120928       if POS>1  ;
030700120928       wlod=%replace(' ':wlod :pos: 7) ;
030800120928       endif  ;
030900120927
031000120927
031100120927       // lettura provincie italia
031200120927       setll (1:'PR') tabel  ;
031300120927       reade (1:'PR') tabel  ;
031400120927       dow not %eof(tabel00f)  ;
031500120927       if tblflg=' ' ;
031600120927       if %subst(tbluni: 23: 1)='I'  ;
031700120927       kprd=%subst(tblkey:1:2)  ;
031800120927       EXSR CERCA_PRO     ;
031900120927       endif  ;
032000120927       endif  ;
032100120927
032200120927       reade (1:'PR') tabel  ;
032300120927       enddo  ;
032400120927
032500120927       kprd='TO'   ;
032600120927       kprd='PG'   ;
032700120927       kprd='FR'   ;
032800120927       kprd='CN'   ;
032900120927       kprd='MI'   ;
033000120927       kprd='GE'   ;
033100120927       kprd='MB'   ;
033200120927       kprd='VA'   ;
033300120927       kprd='CO'   ;
033400120927       kprd='BG'   ;
033500120927       kprd='BS'   ;
033600120927       kprd='CR'   ;
033700120927
033800120927       if wprd='  '   ;
033900120927       clear tisi95ds  ;
034000120927       i95cap=vabcad  ;
034100120927       i95tcn='4'  ;
034200120927       i95loc=wlod  ;
034300120928       ChkCapLocalita     (tisi95ds)  ;
034400120928       if o95err=' ' and o95lid>='3' and o95lia>'0'  ;
034500120928       wprd=o95prv ;
034600120928       endif  ;
034700120928       endif  ;
034800120927
034900120927       except loc   ;
035000120927       endif  ;
035100120927
035200120927       ENDSR  ;
035300120927       //--------------------------------------
035400120927       BEGSR  Cerca_PRO   ;
035500120927
035600120927       Pos=%scan('- '+kprd
035700120927                     +' ' :WLod)  ;
035800120927       if pos>0   ;
035900120927       wprd=kprd                   ;
036000120927       wlod=%replace(' ':wlod :pos: 10) ;
036100120927       endif  ;
036200120927
036300120927       Pos=%scan('-'+kprd
036400120927                    +' '  :WLod)  ;
036500120927       if pos>0   ;
036600120927       wprd=kprd                   ;
036700120927       wlod=%replace(' ':wlod :pos: 10) ;
036800120927       endif  ;
036900120927
037000120927       Pos=%scan('-'+kprd
037100120927                    +'-'  :WLod)  ;
037200120927       if pos>0   ;
037300120927       wprd=kprd                   ;
037400120927       wlod=%replace(' ':wlod :pos: 10) ;
037500120927       endif  ;
037600120927
037700120927       Pos=%scan('( '+kprd+' )':WLod)  ;
037800120927       if pos>0   ;
037900120927       wprd=kprd                   ;
038000120927       wlod=%replace(' ':wlod :pos: 10) ;
038100120927       endif  ;
038200120927
038300120927       Pos=%scan('( '+kprd+')':WLod)  ;
038400120927       if pos>0   ;
038500120927       wprd=kprd                   ;
038600120927       wlod=%replace(' ':wlod :pos: 10) ;
038700120927       endif  ;
038800120927
038900120927       Pos=%scan('/ '+kprd+')':WLod)  ;
039000120927       if pos>0   ;
039100120927       wprd=kprd                   ;
039200120927       wlod=%replace(' ':wlod :pos: 10) ;
039300120927       endif  ;
039400120927
039500120927       Pos=%scan('/ '+kprd+' ':WLod)  ;
039600120927       if pos>0   ;
039700120927       wprd=kprd                   ;
039800120927       wlod=%replace(' ':wlod :pos: 10) ;
039900120927       endif  ;
040000120927
040100120927       Pos=%scan(', '+kprd+' ':WLod)  ;
040200120927       if pos>0   ;
040300120927       wprd=kprd                   ;
040400120927       wlod=%replace(' ':wlod :pos: 10) ;
040500120927       endif  ;
040600120927
040700120927       Pos=%scan(',  '+kprd+' ':WLod)  ;
040800120927       if pos>0   ;
040900120927       wprd=kprd                   ;
041000120927       wlod=%replace(' ':wlod :pos: 10) ;
041100120927       endif  ;
041200120927
041300120927       Pos=%scan(',  '+kprd+')':WLod)  ;
041400120927       if pos>0   ;
041500120927       wprd=kprd                   ;
041600120927       wlod=%replace(' ':wlod :pos: 10) ;
041700120927       endif  ;
041800120927
041900120927       Pos=%scan(' '+kprd+' ':wlod)  ;
042000120927       if pos>0   ;
042100120927       wprd=kprd                   ;
042200120927       wlod=%replace(' ':wlod :pos: 10) ;
042300120927       endif  ;
042400120927
042500120927       ENDSR  ;
042600120927
042700071116      /end-free
042800120927     OQSYSPRT   E            loc         1
042900120927     O                                              'localita :'
043000120928     O                       i_cad5           +   1
043100120928     O                       i_lod            +   1
043200120927     O                       wlod             +   1
043300120927     o                                        +   1 'prov:'
043400120927     O                       wprd             +   1
