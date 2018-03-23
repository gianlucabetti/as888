000100120307     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio) BNDDIR('TIS')
000200150923     h dftactgrp(*no) actgrp('QILE')
000300120306     H* tis7st10r *--------------------------------------------------*
000400120306     H*         Calcolo tassazione preventivo cliente
000500000000     H*--------------------------------------------------------------*
000600120308     Fazorg01l  IF   E           K DISK
000700941103     FTABEL00F  IF   E           K DISK
000800120305     Ftntbe11l  IF   E           K DISK
000900120315     Ftivss01l  IF   E           K DISK
001000051121      *------------------------------------------------------------------------*
001100120309     d kpjba         e ds
001200120309     d ds3idp        e ds
001300120309     d ds5e          e ds
001400120314     d dged          e ds
001500120327     d Ddfe          e ds
001600120314     d dcppflo       e ds
001700120315     d fnlv12ds      e ds
001800120315     d fnlv13ds      e ds
001900120308     d tisi95ds      e ds
002000120309     d tisi97ds      e ds
002100120309     d trul73ds      e ds
002200170119     d trul22ds      e ds
002300120315      // - Ricerca unificante padre
002400120315     d TIBS10ds      e ds                  inz
002500120315     d  skc                   21   5520    dim(500)
002600120301     d
002700120306     D tis7st10i     E DS                  prefix(i10_) INZ(*EXTDFT)
002800120306     D tis7st10o     E DS                  prefix(o10_) INZ(*EXTDFT)
002900120323     d
003000120323     d fnlv55ds      e ds                  inz
003100120308
003200120314     D* DS "TNSF20R" - TASSAZIONE SINGOLA BOLLA
003300120314     D DTAS01        E DS
003400120314     D DTAS          E DS
003500120314     D DTASV         E DS
003600120314     D  VSV                    1     20    DIM(20)                              SIGLA  VARIE
003700120314     D  VAR                   21    140P 3 DIM(20)
003800120302
003900120301     D rqsOpCode       S             10A
004000120301     D rqsData         S           9999A
004100120301     D rqsDataLen      S              4P 0
004200120301     D rpyOpCode       S             10A
004300120301     D rpyData         S           9999A
004400120301     D rpyDataLen      S              4P 0
004500120301     d
004600120301     d Datasys         s               d   datfmt(*iso) inz(*sys)
004700120301     d Dataiso         s               d   datfmt(*iso)
004800120301     d Dataeur         s               d   datfmt(*eur)
004900120301     d Datarif         s              8  0
005000120301     d codut           s              1  0
005100120301     d XX              s              3  0
005200120301     d werror          s              1  0
005300120305     d klin            s              1  0
005400120315     d wksu            s              8  0
005500120315     d wksc            s             11
005600120315     d Indx            s              4  0
005700120306     d Tipoporto       s              1
005800120315     d Wtipoleg        s              2
005900120309     d Werr            s              1
006000120323     d Wtipo           s              1
006100120315     d Ksun            s              9
006200120312     d messag          s            250
006300120307     d LINGUA          s                   like(I10_LINISO2)
006400120309     d wloc            s                   like(I10_idlom)
006500120309     d wcap            s                   like(I10_idcam)
006600120309     d wnar            s                   like(I10_idnzm)
006700120314     d Mitcts          s                   like(o95cts)
006800120314     d Mitiso          s                   like(o95iso)
006900120314     d Mitlnp          s                   like(o95lna)
007000120323     d Wttc            s              5
007100120323     d WttcCAP         s              5
007200120323     d C               s              3  0
007300120327     d DueCTR          s              2  0
007400170120     d wlna            s              3  0
007500120301     ***************************************************************************
007600120306     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
007700120306     D  msgId                         7A   CONST
007800120306     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
007900120306     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
008000120306     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
008100120306     D                                     VARYING
008200120306     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
008300120306     D                                     VARYING
008400120306     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
008500120306     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
008600120306     ***************************************************************************
008700120301     ** Costanti.
008800120301     ***************************************************************************
008900120301     D ESITO_ERROR...
009000120301     D                 C                   -1
009100120301     D ESITO_OK...
009200120301     D                 C                   0
009300120301     D QTEMP...
009400120301     D                 C                   'QTEMP'
009500120301     D RPYOPCODE_DONE...
009600120315     D                 C                   'DONE '
009700120301     D RPYOPCODE_ERROR...
009800120301     D                 C                   'ERROR'
009900120306     D RQSOPCODE_GETTAMCALC...
010000120306     D                 C                   'GETTAMCALC'
010100120301
010200120309      *---------------------------------------------------------------*
010300170119      /COPY GAITRASRC/SRCPROTOPR,TRUL73R
010400120315      /COPY GAITRASRC/SRCPROTOPR,tibs10r
010500120323      /COPY GAITRASRC/SRCPROTOPR,fnlv55r
010600120309      *---------------------------------------------------------------*
010700000000     C     *ENTRY        PLIST
010800120301     C                   PARM                    rqsOpCode
010900120301     C                   PARM                    rqsData
011000120301     C                   PARM                    rqsDataLen
011100120301     C                   PARM                    rpyOpCode
011200120301     C                   PARM                    rpyData
011300120301     C                   PARM                    rpyDataLen
011400120301     c
011500120301     c                   clear                   rpyData
011600120301     c                   clear                   rpyDataLen
011700120315     c                   clear                   rpyopcode
011800120301     c
011900120302     c                   eval      rpyopcode=RPYOPCODE_ERROR
012000120302
012100120301     c                   exsr      Contrdati
012200120302     c
012300120306    1c                   if        Werror= ESITO_OK
012400120306     c                   exsr      contrTas
012500120314     c                   endif
012600120306
012700120314    1c                   if        Werror= ESITO_OK
012800120306     c                   exsr      Tassazione
012900120302    1c                   endif
013000120302
013100120302    1c                   if        Werror= ESITO_OK
013200120302     c                   eval      rpyopcode=RPYOPCODE_DONE
013300120302
013400120307     c                   eval      rpyData = tis7st10o
013500120306     c                   eval      rpyDataLen = %SIZE(tis7st10o)
013600120302     c
013700120302     c                   else
013800120320     c****               dump(a)
013900120314     c                   eval      rpyDataLen = %len(%trim(rpydata))
014000120302    1c                   endif
014100900323     C*
014200120302     C                   return
014300120301     c*------------------------------------------------------------------*
014400120301     C*   controlli dati passati
014500120301     c*------------------------------------------------------------------*
014600120301     c     ContrDATI     BEGSR
014700120301     c                   clear                   Werror
014800120315     c                   clear                   Wksu
014900120314     c                   clear                   TipoPorto
015000120301      /FREE
015100120306 1     IF rqsOpCode <> RQSOPCODE_GETTAMCALC     ;
015200120306         WError=  ESITO_ERROR;
015300120306         rpyData = 'rqsOpCode errato o non gestito' ;
015400120306         leavesr   ;
015500120301 1     ENDIF;
015600120301
015700120301
015800120306       reset tis7st10i;
015900120302
016000120309 1     SELECT;
016100120306         WHEN %SUBST(rqsData : 1 : 10) = i10_formato;
016200120306           tis7st10i = %SUBST(rqsData : 1 : rqsDataLen);
016300120302         OTHER;
016400120302           Werror= ESITO_ERROR;
016500120306         rpyData = 'Richiamo errato' ;
016600120306         leavesr   ;
016700120309 1     ENDSL;
016800120302
016900120309 1     monitor  ;
017000120302
017100120315       // Se non presente passo login per otrnare alla videata di
017200120315       //  collegamento
017300120306 2     IF i10_idcliente <= *zeros ;
017400120302         Werror = ESITO_ERROR;
017500120315         rpyopcode='LOGIN' ;
017600120315         clear rpyData  ;
017700120315         leavesr   ;
017800120309 2     endif   ;
017900120309
018000120315       on-error ;
018100120315         Werror = ESITO_ERROR;
018200120315         rpyopcode='LOGIN' ;
018300120315         clear rpyData  ;
018400120315         leavesr   ;
018500120309 1     endmon  ;
018600120315
018700120315 1     monitor  ;
018800120315       wksu=%int(i10_ksu) ;
018900120315
019000120315       on-error ;
019100120315         Werror = ESITO_ERROR;
019200120315         rpyopcode='LOGIN' ;
019300120315         clear rpyData  ;
019400120315         leavesr   ;
019500120315 1     endmon  ;
019600120315
019700120315 1     IF wksu   <= *zeros ;
019800120315         Werror = ESITO_ERROR;
019900120315         rpyopcode='LOGIN' ;
020000120315         clear rpyData  ;
020100120315         leavesr   ;
020200120315 1     endif   ;
020300120315
020400120315 1     monitor  ;
020500120315
020600120315       // Se non presente passo login per otrnare alla videata di
020700120315       //  collegamento
020800120315 2     IF i10_sun <= *zeros ;
020900120315         Werror = ESITO_ERROR;
021000120315         rpyopcode='LOGIN' ;
021100120315         clear rpyData  ;
021200120315         leavesr   ;
021300120315 2     endif   ;
021400120315
021500120315       on-error ;
021600120315         Werror = ESITO_ERROR;
021700120315         rpyopcode='LOGIN' ;
021800120315         clear rpyData  ;
021900120315         leavesr   ;
022000120315 1     endmon  ;
022100120302
022200120309 1     monitor  ;
022300120306 2     IF i10_idcodtar  <  *zeros ;
022400120302         Werror = ESITO_ERROR;
022500120312         clear messag ;
022600120313         messag =  rtvMsgLang('TIS0705':lingua) ;
022700120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
022800120306         leavesr   ;
022900120302 2     ENDIF;
023000120305
023100120302       on-error ;
023200120302           Werror= ESITO_ERROR;
023300120312         clear messag ;
023400120313         messag =  rtvMsgLang('TIS0705':lingua) ;
023500120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
023600120306         leavesr   ;
023700120309 1     endmon  ;
023800120302
023900120309 1     if i10_tipocli<>'M' and
024000120306          i10_tipocli<>'D' and
024100120306          i10_tipocli<>'O'    ;
024200120302           Werror= ESITO_ERROR;
024300120309         rpyData = %trim(rpyData) +
024400120309                   'Non indicato se cliente è Mittente Destinatario +
024500120309                    o Ordinante.<br>'   ;
024600120309 1     endif  ;
024700120306
024800120306       // Imposto tipo porto
024900120309 1     if i10_tipocli<>'M' ;
025000120307       Tipoporto='A'   ;
025100120306       else  ;
025200120307       Tipoporto='F'   ;
025300120309 1     endif  ;
025400120306
025500120306
025600120306       lingua= i10_liniso2        ;
025700120305
025800120305       // Controllo e selezione lingua
025900120309 1     select ;
026000120305
026100120305       when  lingua ='en'   ;
026200120305        klin =2   ;
026300120305       when  lingua ='fr'   ;
026400120305        klin =3   ;
026500120305       when  lingua ='de'   ;
026600120305        klin =4   ;
026700120305       when  lingua ='it' or lingua='  '  ;
026800120305        klin =1   ;
026900120305       other ;
027000120305           Werror= ESITO_ERROR;
027100120309         rpyData = %trim(rpyData) +
027200120312                   'Codice lingua errato.<br>' ;
027300120309 1     endsl  ;
027400120301
027500120314       // Reperisco divisa corrente
027600120314       tbeke1='1              '          ;
027700120314       chain (' ':'GED':tbeke1)  tntbe11l    ;
027800120314       if %found(tntbe11l)    ;
027900120314       dged=tbeuni  ;
028000120314       endif  ;
028100120314
028200120301      /END-FREE
028300120301     c                   ENDSR
028400120302     c*------------------------------------------------------------------*
028500120306     C*   controllo ai fini della tassazione
028600120302     c*------------------------------------------------------------------*
028700120306     c     contrTas      BEGSR
028800120302      /FREE
028900120305
029000120306       reset tis7st10o   ;
029100120306       clear xx ;
029200120309       clear ds3idp ;
029300120314       clear mitlnp  ;
029400120314       clear mitiso  ;
029500120314       clear mitcts  ;
029600120315       clear wtipoleg  ;
029700120323       clear wtipo     ;
029800120323       clear ds5e      ;
029900120327       clear ddfe      ;
030000170120       clear wlna      ;
030100120309
030200120309 1     if i10_idnetwork='DPD'   ;
030300120309         clear tblkey  ;
030400120309         tblkey='DPD'   ;
030500120309
030600120309         chain (1 :'3I':tblkey)   tabel00f ;
030700120309 2       if %found(tabel00f)  ;
030800120309          ds3idp=tbluni   ;
030900120309 2       endif   ;
031000120309 1     endif   ;
031100120327
031200120327 1     if i10_idnetwork='FED'   ;
031300120327       clear   tbeke1   ;
031400120327       tbeke1='FED'   ;
031500120327       chain (' ':'DFE':tbeke1)  tntbe11l    ;
031600120305
031700120327       if %found(tntbe11l)    ;
031800120327       dDFE=tbeuni  ;
031900120327       endif  ;
032000120327       endif   ;
032100120327
032200120309       // Prima controllo i campi che mi devono arrivare numerici
032300120309
032400120308       // DATA SPEDIZIONE
032500120306
0326001203061      if i10_dtspediz<>*blanks and i10_dtspediz<>*zeros ;
032700120302
0328001203092      monitor  ;
032900120306         dataiso=%date(i10_DTSPEDIZ:  *eur)   ;
033000120302         on-error ;
033100120302           Werror= ESITO_ERROR;
033200120312         rpyData =  rtvMsgLang('TIS0121':lingua) ;
033300120312         rpydata=%trim(rpydata)+'.<br>' ;
033400120309 2     endmon  ;
033500120302
033600120302x1     else    ;
033700120302       dataiso=datasys   ;
033800120302 1     endif     ;
033900120309
034000120309 1     if werror =esito_ok  ;
034100120309         datarif=%dec(dataiso)   ;
034200120309 1     endif  ;
034300120309
034400120309       // LINEA DI PARTENZA
034500120309 1     Monitor  ;
034600120309 2     IF  i10_idlnp  <  *zeros ;
034700120309         Werror = ESITO_ERROR;
034800120312         clear messag ;
034900120313         messag =  rtvMsgLang('TIS0699':lingua) ;
035000120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
035100120309 2     ENDIF;
035200120309
035300120309       on-error ;
035400120309           Werror= ESITO_ERROR;
035500120312         clear messag ;
035600120313         messag =  rtvMsgLang('TIS0699':lingua) ;
035700120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
035800120309 1     endmon  ;
035900120309
036000120309       //  COLLI
036100120309 1     Monitor  ;
036200120309 2     IF  i10_idncl  <= *zeros ;
036300120309         Werror = ESITO_ERROR;
036400120312         clear messag ;
036500120312         messag=   rtvMsgLang('TIS0702':lingua)  ;
036600120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
036700120309 2     ENDIF;
036800120309
036900120309       on-error ;
037000120309           Werror= ESITO_ERROR;
037100120312         clear messag ;
037200120312         messag  = rtvMsgLang('TIS0702':lingua)  ;
037300120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
037400120309 1     endmon  ;
037500120309
037600120309       // PESO
037700120309 1     Monitor  ;
037800120309 2     IF  i10_idpkf  <= *zeros ;
037900120309         Werror = ESITO_ERROR;
038000120312           clear messag ;
038100120313           messag =rtvMsgLang('TIS0703':lingua) ;
038200120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
038300120309 2     ENDIF;
038400120309
038500120309       on-error ;
038600120309           Werror= ESITO_ERROR;
038700120312           clear messag ;
038800120313         messag  = rtvMsgLang('TIS0703':lingua) ;
038900120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
039000120309 1     endmon  ;
039100120309
039200120309       // VOLUME
039300120309 1     Monitor  ;
039400120323 2     //   IF  i10_idvlf  <= *zeros ;
039500120323 2     IF  i10_idvlf  <  *zeros ;
039600120309         Werror = ESITO_ERROR;
039700120312           clear messag ;
039800120313         messag  = rtvMsgLang('TIS0704':lingua) ;
039900120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
040000120309 2     ENDIF;
040100120309
040200120309       on-error ;
040300120309           Werror= ESITO_ERROR;
040400120312           clear messag ;
040500120312         messag  = rtvMsgLang('TIS0704':lingua)+'.<br>' ;
040600120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
040700120309 1     endmon  ;
040800120309
040900120309       // C/ASSEGNO
041000120309 1     Monitor  ;
041100120309 2     IF  i10_idcas  <  *zeros ;
041200120309         Werror = ESITO_ERROR;
041300120312           clear messag ;
041400120312         messag =  rtvMsgLang('TIS0269':lingua)+'.<br>' ;
041500120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
041600120309         WErr='1'  ;
041700120309 2     ENDIF;
041800120309
041900120309       on-error ;
042000120309           Werror= ESITO_ERROR;
042100120312           clear messag ;
042200120312         messag =  rtvMsgLang('TIS0269':lingua)+'.<br>' ;
042300120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
042400120309 1     endmon  ;
042500120309
042600120309       // Importo ASSICURARE
042700120309 1     Monitor  ;
042800120309 2     IF  i10_idias  <  *zeros ;
042900120309         Werror = ESITO_ERROR;
043000120312           clear messag ;
043100120312         messag =  rtvMsgLang('TIS0269':lingua)+'.<br>' ;
043200120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
043300120309 2     ENDIF;
043400120309
043500120309       on-error ;
043600120309           Werror= ESITO_ERROR;
043700120312           clear messag ;
043800120312            messag=rtvMsgLang('TIS0269':lingua)+'.<br>' ;
043900120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
044000120309 1     endmon  ;
044100120309
044200120309       // Quantità da FATTURARE
044300120309 1     Monitor  ;
044400170119 2     IF  i10_idqta  <  *zeros ;
044500120309         Werror = ESITO_ERROR;
044600120312           clear messag ;
044700120312         messag =  rtvMsgLang('TIS0269':lingua)+'.<br>' ;
044800120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
044900120309 2     ENDIF;
045000120309
045100120309       on-error ;
045200120309           Werror= ESITO_ERROR;
045300120312           clear messag ;
045400120313         messag =  rtvMsgLang('TIS0269':lingua) ;
045500120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
045600120309 1     endmon  ;
045700120302
045800120309       // Tipo SERVIZIO "C" unico ammesso per export
045900120309
046000120309 1     if tipoporto='F' and i10_idtsp<>'C' and i10_idnetwork<>*blanks   ;
046100120309         Werror = ESITO_ERROR;
046200120312           clear messag ;
046300120313         messag=   rtvMsgLang('TIS0706':lingua) ;
046400120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
046500120309 1     ENDIF;
046600120313
046700120313       // se non è in tabella --> errore
046800120313       clear tblkey  ;
046900120314       tblkey=i10_idtsp   ;
047000120313
047100120313       chain (klin:'5E':tblkey) tabel00f   ;
047200120313 1     if not %found(tabel00f)  ;
047300120314         Werror = ESITO_ERROR;
047400120314         clear messag ;
047500120313         messag=   rtvMsgLang('TIS0551':lingua) ;
047600120313         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
047700120323         else  ;
047800120323         ds5e=tbluni   ;
047900120313 1     endif  ;
048000120423
048100120423       // Bolle Fedex: impossibile spedizione in assegnato
048200120423 1     if i10_idnetwork='FED' and tipoporto='A' ;
048300120423         Werror = ESITO_ERROR;
048400120423         clear messag ;
048500120423         messag=   rtvMsgLang('TIS0717':lingua) ;
048600120423         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
048700120423       endif  ;
048800120309
048900120313       // per questi ERRORI  devo uscire subito dal pgm
049000120309 1     if Werror = ESITO_ERROR;
049100120309         leavesr  ;
049200120309 1     endif  ;
049300120315
049400120315         // Verifico congruenza dei codici del cliente che richiede la tassazione
049500120315
049600120315         ksun=%editc(i10_sun:'X')  ;
049700120315         chain (Ksun:'TT' ) tivss01l   ;
049800120315 1       if %found( tivss01l) and datarif>=vssdde and
049900120315            datarif<=vssdsc    ;
050000120315            Wtipoleg=vsstle  ;
050100120315         else  ;
050200120315          Werror = ESITO_ERROR;
050300120315          rpyopcode='LOGIN' ;
050400120315          clear rpyData  ;
050500120315          leavesr   ;
050600120315 1       endif     ;
050700120315
050800120315       // Prendo codice unificante e carico cod collegati figli
050900120315           clear tibs10ds ;
051000120315           d10tle=wtipoleg  ;
051100120315           d10paf='F'    ;
051200120315           d10cod=wksu   ;
051300120315           callp TIBS10R (tibs10ds)   ;
051400120315
051500120315  1        if d10err<>*blanks   ;
051600120315           // Se errore verifico se è figlio
051700120315           clear tibs10ds ;
051800120315           d10tle=wtipoleg    ;
051900120315           d10paf='P'    ;
052000120315           d10cod=wksu;
052100120315           callp TIBS10R (tibs10ds)   ;
052200120315
052300120315  2        if d10err<>*blanks   ;
052400120315             // Imposto nella skiera e come padre solo se stesso
052500120315             skc(1)='000'+%editc(wksu:'X');
052600120315             d10cop=wksu                  ;
052700120315  2        endif                ;
052800120315  1        endif ;
052900120315
053000120315           wksc='0000'+%editc(i10_idcliente:'X');
053100120315           Indx=%lookup(wksc:skc)   ;
053200120315  1        if Indx=0 ;
053300120315            Werror = ESITO_ERROR;
053400120315            rpyopcode='LOGIN' ;
053500120315            clear rpyData  ;
053600120315            leavesr   ;
053700120315  1        endif  ;
053800120308
053900120309       // CONTROLLO LIMITI DI PESO/COLLI/VOLUME
054000120309       clear trul73ds   ;
054100120309       i73tsp=i10_idtsp   ;
054200120309       i73psk=i10_idpkf  ;
054300120309       i73vmc=i10_idvlf ;
054400120309       i73ncl=i10_idncl ;
054500120309       callp TRUL73R  (trul73ds)   ;
054600120309
054700120309       // errore colli
054800120309       if  o73fclm='1'       ;
054900120309         Werror = ESITO_ERROR;
055000120314         clear messag ;
055100120326         messag =  rtvMsgLang('TIS0707':lingua:§5edes) ;
055200120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
055300120309 1     ENDIF;
055400120309       // Errore peso
055500120309       if  o73fkgm='1'       ;
055600120309         Werror = ESITO_ERROR;
055700120312           clear messag ;
055800120326         messag =  rtvMsgLang('TIS0708':lingua:§5edes) ;
055900120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
056000120309 1     ENDIF;
056100120309       // Errore volume
056200120309       if  o73fmcm='1'       ;
056300120309         Werror = ESITO_ERROR;
056400120312           clear messag ;
056500120326         messag =  rtvMsgLang('TIS0709':lingua:§5edes) ;
056600120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
056700120309 1     ENDIF;
056800120309
056900120309       //  CONTROLLI COLLI E PESO PER DPD
057000120309  1    IF I10_idNETWORK='DPD'    ;
057100120309
057200120314  2      IF I10_IDncl<>1  or i10_idpkf>§3ipkd  ;
057300120309         Werror = ESITO_ERROR;
057400120312           clear messag ;
057500120323         messag =  rtvMsgLang('TIS0710':lingua:%editc(§3ipkd:'4'))  ;
057600120323         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
057700120309  2      endif  ;
057800120309
057900120309  1    ENDIF   ;
058000120309
058100120309       // secondo gruppo di errori che prevede l'uscita dal pgm
058200120309 1     if Werror = ESITO_ERROR;
058300120309         leavesr  ;
058400120309 1     endif  ;
058500120309
058600120327       //  CONTROLLO PESO PER FEDEX documenti
058700120327  1    IF I10_idNETWORK='FED'    ;
058800120327         duectr=%int(%subst(%editc(i10_idcodtar :'X'):2:2))  ;
058900120327  2      if i10_idpkf> §DfePkgD and dueCtr >= §DfeDalD
059000120327            and dueCtr <= §DfeAlD     ;
059100120327         Werror = ESITO_ERROR;
059200120327           clear messag ;
059300120327         messag =  rtvMsgLang('TIS0716':lingua:%editc(§dfepkgd:'4'))  ;
059400120327         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
059500120327  2      endif  ;
059600120327  1    endif  ;
059700120327
059800120309
059900120308       // LINEA DI PARTENZA
060000120308
060100120308       // Per i franchi se manca la linea di partenza --> errore  ;
060200120308
060300120309 1     IF Tipoporto='F' and i10_idlnp  <= *zeros ;
060400120308         Werror = ESITO_ERROR;
060500120312           clear messag ;
060600120313         messag =  rtvMsgLang('TIS0699':lingua) ;
060700120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
060800120309 1     ENDIF;
060900120308
061000120309 1       IF i10_idlnp  >  *zeros  ;
061100120308           chain  i10_idlnp  azorg01l ;
061200120309 2         if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A');
061300120308             Werror= ESITO_ERROR;
061400120312           clear messag ;
061500120313         messag =      rtvMsgLang('TIS0699':lingua) ;
061600120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
061700120309 2         endif  ;
061800120309 1       endif  ;
061900120301
062000120308       // MITTENTE: nazione/cap/località solo per asssegnati
062100120308        WERR='0'  ;
062200120313        clear   tisi97ds     ;
062300120315        clear   fnlv12ds  ;
062400120314        clear  wnar ;
062500120314        clear  wcap ;
062600120314        clear  wloc ;
062700120323        wtipo='M'  ;
062800120309
062900120309 1     if tipoporto='A' and i10_idcam=*blanks  ;
063000120308         Werror = ESITO_ERROR;
063100120312           clear messag ;
063200120313         messag = rtvMsgLang('TIS0237':lingua) ;
063300120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
063400120308 1     ENDIF;
063500120308
063600120309 1     if i10_idcam<>*blanks   ;
063700120312         if i10_idnzm='IT'  ;
063800120312         clear wnar  ;
063900120312         else  ;
064000120309         wnar=i10_idnzm   ;
064100120312         endif  ;
064200120309         wcap=i10_idcam  ;
064300120309         exsr   CHKcap   ;
064400120309
064500120309 2       if Werr='1'   ;
064600120314           Werror= ESITO_ERROR;
064700120312           clear messag ;
064800120319         messag =      rtvMsgLang('TIS0700':lingua:i10_idcam) ;
064900120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
065000120309         Werr='1'    ;
065100120309 2       endif   ;
065200120309
065300120309
065400120309 2      if i10_idlom<>*blanks  and werr='0'  ;
065500120312           if i10_idnzm='IT'  ;
065600120312            clear wnar  ;
065700120312           else  ;
065800120309           wnar=i10_idnzm   ;
065900120312           endif   ;
066000120309           wcap=i10_idcam  ;
066100120309           wloc=i10_idlom  ;
066200120309           exsr   CHKclp   ;
066300120309
066400120309 3       if Werr='1'   ;
066500120314           Werror= ESITO_ERROR;
066600120312           clear messag ;
066700120319         messag =      rtvMsgLang('TIS0701':lingua:wloc) ;
066800120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
066900120309 3       endif   ;
067000120309 2       endif   ;
067100120315
067200120315
067300120315           // Se presente network, controllo se utilizzabile per ritiri
067400120315 2           if werr='0' and i10_idnetwork<>*blanks   ;
067500120315              i97ntw =i10_idnetwork  ;
067600120315              i12lang=lingua  ;
067700120315      /END-FREE
067800120315
067900120315     C                   CALL      'FNLV12R'
068000120315     C                   PARM                    fnlv12ds
068100120315     C                   PARM                    tisi95ds
068200120315     C                   PARM                    tisi97ds
068300120315
068400120315      /FREE
068500120315 3           if o12err<>*blanks   ;
068600120315              Werror= ESITO_ERROR;
068700120315              rpyData = %trim(rpyData)  + %trim(o12msg)+'.<br>' ;
068800120315              WErr='1'  ;
068900120315 3           endif   ;
069000120315 2           endif   ;
069100120315
069200120315
069300120313        // Se non ci sono errori, memorizzo linea di partenza se manca cod tas
069400120313        //  e flag inoltro
069500120313 2          if werr='0'   ;
069600120314          mitcts=o95cts  ;
069700120314          mitiso=o95iso  ;
069800120314          mitlnp=o95lna  ;
069900120313 2          endif   ;
070000120313 1       endif   ;
070100120312
070200120312       // DESTINATARIO: nazione/cap/località solo per asssegnati
070300120312        WERR='0'  ;
070400120315        clear   tisi97ds     ;
070500120314        clear  wnar ;
070600120314        clear  wcap ;
070700120314        clear  wloc ;
070800120323        wtipo='D'  ;
070900120313
071000120312         if i10_idnzd='IT'  ;
071100120312         clear wnar  ;
071200120312         else  ;
071300120312         wnar=i10_idnzd   ;
071400120312         endif  ;
071500120312         wcap=i10_idcad  ;
071600120313
071700120312         exsr   CHKcap   ;
071800120312
071900120312 1       if Werr='1'   ;
072000120312             Werror= ESITO_ERROR;
072100120312           clear messag ;
072200120319         messag =      rtvMsgLang('TIS0700':lingua:i10_idcad) ;
072300120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
072400120312         Werr='1'    ;
072500120312 1       endif   ;
072600120312
072700120312
072800120312 1      if i10_idlod<>*blanks  and werr='0'  ;
072900120312           if i10_idnzd='IT'  ;
073000120312            clear wnar  ;
073100120312           else  ;
073200120312           wnar=i10_idnzd   ;
073300120312           endif   ;
073400120312           wcap=i10_idcad  ;
073500120312           wloc=i10_idlod  ;
073600120313
073700120312           exsr   CHKclp   ;
073800120312
073900120312 2       if Werr='1'   ;
074000120314           Werror= ESITO_ERROR;
074100120312           clear messag ;
074200120319         messag =      rtvMsgLang('TIS0701':lingua:wloc)         ;
074300120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
074400170120         else    ;
074500170120         wlna=o95lna  ;
074600120312 2       endif   ;
074700120312 1       endif   ;
074800120313
074900120313       // Se presente network, controllo se utilizzabile
075000120315 1       if werr='0' and i10_idnetwork<>*blanks   ;
075100120313
075200170120         CLEAR WTIPO  ;
075300120314          i97ntw =i10_idnetwork  ;
075400120314          exsr chkcap   ;
075500120314
075600120830 2        if werr='1'         ;
075700120314            Werror= ESITO_ERROR;
075800120314            dcppflo=o97flo   ;
075900120314            clear messag ;
076000120314            messag =      o13msg          ;
076100120314            rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
076200120314            if §cppall='S'   ;
076300120314            endif  ;
076400120315 2       endif  ;
076500120315 1       endif  ;
076600120323
076700120323       // controllo tipo servizio se ci sono limitazioni di codici tempo
076800120323       clear tblkey  ;
076900120323       clear  ds5e  ;
077000120323       tblkey=i10_idtsp  ;
077100120323
077200120323       chain (klin :'5E':tblkey)   tabel00f ;
077300120323 1     if %found(tabel00f)  ;
077400120323        ds5e=tbluni   ;
077500120323        o10_descrtsp= §5edes  ;
077600120323 1     endif   ;
077700120323
077800120323      /END-FREE
077900120323     c
078000120323    1c                   if        werr='0' and §5ettc<>*blanks
078100120323     c* controllo posizione per posizione escudendo le "x"
078200120323     c                   eval      Wttc=§5ettc
078300120323     c                   eval      WttcCAP=%editc(o95ttc:'X')
078400120323     c                   z-add     1             c
078500120323    2c                   do        5             c
078600120323    3c                   if        %subst(wttc:c:1)<>'x'
078700120323     c                             and
078800120323     c                             %subst(wttc:c:1)<>%subst(wttcCAP:c:1)
078900120323     c                   eval      Werror= ESITO_ERROR
079000120323     c                   clear                   messag
079100120323     c                   eval      messag =
079200120323     c                                   rtvMsgLang('TIS0715':LINGUA:§5edes)
079300120323     c                   eval      rpyData=%trim(rpyData)+%trim(messag)+'.<br>'
079400120323    3c                   endif
079500120323    2c                   enddo
079600120323    1c                   endif
079700170119     c
079800170120     c                   if        i10_idias>*zeros
079900170120     c                   clear                   trul22ds
080000170119     c
080100170119IF  3C     Tipoporto     IFEQ      'F'                                          *FRANCO->DATI DESTIN
080200170119     C                   MOVEL     'F1'          i22cbo                         *TIPO BOLLA
080300170119X   3C                   ELSE                                                   *ASSEGNATO->DATI MIT
080400170119     C                   MOVEL     'A2'          i22cbo
080500170120     c                   endif
080600170119     C                   MOVEL     i10_idtsp     i22tsp
080700170119     c                   if        i10_idlnp>*zeros
080800170119     C                   Z-ADD     i10_idlnp     i22LNP                         *LINEA PARTENZA
080900170119     c                   else
081000170119     c                   z-add     mitlnp        i22lnp
081100170119     c                   endif
081200170119     C                   MOVEL     i10_idnzm     i22NZM                         *NAZIONE MITTENTE
081300170120     C                   Z-ADD     wlna          i22LNA                         *LINEA ARRIVO
081400170119     C                   MOVEL     i10_idnzd     i22NZD                         *NAZIONE DESTINO
081500170119     C                   MOVEL     i10_idcliente i22KSC
081600170119     C                   MOVEL     i10_idcodtar  i22CTR
081700170120     C                   Z-ADD     i10_idias     i22IMP                         *ASSICURAZIONE
081800170119     C                   MOVEL     §GEDCr        i22div                         *DIVISA NON LA SO
081900170119     c                   call      'TRUL22R'
082000170119     c                   parm                    trul22ds
082100170119
082200170119     C* IMPORTO DA ASSICUARE NON AMMESSO
082300170119     C     O22FMN        IFNE      *BLANKS
082400170119     C     O22FX2        orne      *BLANKS
082500170120     c                   eval      WError=  ESITO_ERROR
082600170120     c                   clear                   messag
082700170119     c                   eval      messag =
082800170119     c                                   rtvMsgLang('TIS0861')
082900170119     c                   eval      rpyData=%trim(rpyData)+%trim(messag)+'.<br>'
083000170119     c                   endif
083100170119     c                   endif
083200120323     c
083300120323     c                   ENDSR
083400120302
083500120323      /FREE
083600120309       //-----------------------------------------------------------------*
083700120309       //  controllo CAP
083800120309       //-----------------------------------------------------------------*
083900120309       BEGSR   CHKcap     ;
084000120313
084100120313       exsr     CAllFNLV13 ;
084200120313
084300120309       if o95err<>*blanks   or
084400120309          (o13err<>*blanks and o13rop=*blanks)   ;
084500120309         WErr='1'  ;
084600120309       endif  ;
084700120309
084800120309       ENDSR    ;
084900120309       //-----------------------------------------------------------------*
085000120309       //  controllo CAP/LOCALITà
085100120309       //-----------------------------------------------------------------*
085200120309       BEGSR   CHKclp     ;
085300120313
085400120313       exsr     CAllFNLV13 ;
085500120309
085600120309       if o95err<>*blanks   or
085700120312          (o13err<>*blanks and o13rop=*blanks)   or
085800120313          (o95lia = '1') or
085900120802          (wnar= *blanks and wloc<>*blanks and o95lid<'3') ;
086000120309         WErr='1'  ;
086100120309       endif  ;
086200120309
086300120309       ENDSR    ;
086400120309
086500120313       //-----------------------------------------------------------------*
086600120313       //  Richiamo pgm fnlv13r
086700120313       //-----------------------------------------------------------------*
086800120314       BEGSR CallFNLV13  ;
086900120314
087000120313       clear fnlv13ds  ;
087100120313       clear tisi95ds  ;
087200120313       i95tcn='3'  ;
087300120314       i95nar=wnar       ;
087400120802
087500120802        // CHIODO per IRLANDA
087600120802       if wnar='IRL'     ;
087700120802        i95cap='EIRE'  ;
087800120802       else  ;
087900120802        i95cap=wcap       ;
088000120802        endif  ;
088100120314       i95loc=wloc       ;
088200120313       i95dat=datarif  ;
088300120313       i95lkg=i10_idpkf  ;
088400120313       i95lmc=i10_idvlf   ;
088500120313       i95tsp=i10_idtsp  ;
088600120313       i95tpo=tipoporto ;
088700120323       if    i10_idffd ='1'   ;
088800120323         i95ffd='S'       ;
088900120323       endif  ;
089000120323
089100120323       // solo per destinatario
089200120323 1     if wtipo='D'  ;
089300120323
089400120323       // calcolo terminal di partenza ddella linea di partenza
089500120323       clear fnlv55ds  ;
089600120323 2      if i10_idlnp>0    ;
089700120323         d55lin=i10_idlnp  ;
089800120323 x2      else  ;
089900120323
090000120323 3       if mitlnp>0   ;
090100120323         d55lin=mitlnp   ;
090200120323 3      endif  ;
090300120323 2      endif  ;
090400120323
090500120323 2      if   d55lin>0  ;
090600120323        i95fre='S'        ;
090700120323        d55tpt='P'  ;
090800120323        d55drf=datarif   ;
090900120323        callp FNLV55R  (fnlv55ds)  ;
091000120323        i95tfp=d55tfp  ;
091100120323 2      endif  ;
091200120323 1      endif  ;
091300120313
091400170120       if (wtipo='M' and  tipoporto= 'A' and wnar<>'  ') or wtipo =' ' or
091500170120       (wtipo= 'D' and  tipoporto='F' and wnar<>'   ' )     ;
091600170120
091700120313       if i10_idnetwork='FED'   ;
091800120313         I95fi1='S'  ;
091900120313       endif    ;
092000120313       if i10_idnetwork='DPD'   ;
092100120313         I95fi1='D'  ;
092200120313       endif    ;
092300170120       endif    ;
092400120313
092500120313       // C/Asssegno
092600120313       if i10_idcas > *zeros   ;
092700120313        i95fca='S'    ;
092800120313       endif   ;
092900120313
093000120313      /END-FREE
093100120313
093200120313     C                   CALL      'FNLV13R'
093300120313     C                   PARM                    KPJBA
093400120313     C                   PARM                    fnlv13ds
093500120313     C                   PARM                    tisi95ds
093600120313     C                   PARM      ' '           flgbac            1
093700120313     C                   PARM                    tisi97ds
093800120322     C                   PARM                    lingua
093900120313     c
094000120313     c                   ENDSR
094100120307     c*------------------------------------------------------------------*
094200120307     C*   Tassazione
094300120307     c*------------------------------------------------------------------*
094400120307     c     Tassazione    BEGSR
094500120314     C                   CLEAR                   DTAS                           *PULISCE DS DI LANCI
094600120314     C                   CLEAR                   DTASV
094700120314     C                   MOVEL     'L'           TASTLA                         *ELABORA E CHIUDE
094800120314     C                   MOVEL     i10_idcliente TASKSC
094900120314     C                   MOVEL     i10_idcodtar  TASCTR
095000120314     C*
095100120314     C* CALCOLA I SINGOLI ELEMENTI DI TASSAZIONE
095200120314IF  3C     Tipoporto     IFEQ      'F'                                          *FRANCO->DATI DESTIN
095300120314     C                   MOVEL     'F1'          TASTBL                         *TIPO BOLLA
095400120314X   3C                   ELSE                                                   *ASSEGNATO->DATI MIT
095500120314     C                   MOVEL     'A2'          TASTBL                         *TIPO BOLLA
095600120314     C                   MOVEL     Mitcts        TASMCT                         *CODICE TASSAZIONE
095700120314     C                   MOVEL     Mitiso        TASFAP                         *FLAG INOLTRO
095800120314E   3C                   ENDIF
095900120314     c* Se consegna a magazzino, flag anteporto='C'
096000120314     c                   if        i10_idRicMag='1'
096100120314     c                   eval      tasfap='C'
096200120314     c                   eval      tasfdn='S'
096300120314     c                   endif
096400120314     c
096500160530     C                   Z-ADD     i10_idncl     TASNCL                         *COLLI originali
096600160530     C                   Z-ADD     i10_idncl     TASNCLB                        *COLLI bolla
096700120314     C                   Z-ADD     i10_idpkf     TASPKF                         *PESO
096800160530     C                   Z-ADD     i10_idpkf     TASPKB                         *PESO
096900120314     C                   Z-ADD     i10_idvlf     TASVLF                         *VOLUME
097000160530     C                   movel     'R'           TASFVF                         *VOLUME
097100120314     C                   Z-ADD     i10_idcas     TASCAS                         *IMPORTO C/ASSEGNO
097200120314     C                   MOVEL     §GEDCr        TASvca                         *DIVISA NON LA SO
097300120314     c* Lasciamo impostato *blanks come "BRT"
097400120314     C*????              MOVEL     D92TIC        TASTIC                         *INCASSO C/ASSENGO
097500120314     C                   MOVEL     §GEDCr        TASvas                         *DIVISA NON LA SO
097600120314     C                   Z-ADD     i10_idias     TASIAS                         *ASSICURAZIONE
097700120314     C                   Z-ADD     i10_idqta     TASqft                         *ASSICURAZIONE
097800120314     C                   Z-ADD     datarif       TASDSP                         *DATA SPEDIZIONE
097900120314     C                   Z-ADD     datarif       TASDCT                         *DATA CAR TAR CAR
098000120314     c                   if        i10_idnzd<>'IT'
098100120314     C                   MOVEL     i10_idnzd     TASNZD                         *NAZIONE DESTINO
098200120314     c                   endif
098300120314     c                   if        i10_idnzm<>'IT'
098400120314     C                   MOVEL     i10_idnzm     TASNZM                         *NAZIONE MITTENTE
098500120314     c                   endif
098600120314     C                   MOVEL     i10_idcad     TASCAD                         *CAP DESTINATARIO
098700120314     c
098800120314     C                   MOVEL     O95CTS        TASCTS                         *CODICE TASSAZIONE
098900120314     C                   MOVEL     O95ISO        TASFIN                         *FLAG INOLTRO
099000120314     C                   MOVEL     §GEDCN        TASDIV                         *DIVISA NON LA SO
099100120314     c* Se fermo deposito flag inoltro = 'C'
099200120314     c                   if        i10_idffd ='1'
099300120314     c                   movel     'C'           tasfin
099400120314     c                   endif
099500120314     C                   MOVEL     i10_idtsp     TASTSP                         *TIPO SERVIZIO
099600120314     c                   if        i10_idlnp>*zeros
099700120314     C                   Z-ADD     i10_idlnp     TASLNP                         *LINEA PARTENZA
099800120314     c                   else
099900120314     c                   z-add     mitlnp        taslnp
100000120314     c                   endif
100100120314     C                   Z-ADD     O95LNA        TASLNA                         *LINEA ARRIVO
100200120314     C* IMPOSTO LA LINEA DI ARRIVA IN RELAZIONE AL NETWROK
100300120314     C**   D92NTW        IFEQ      'DPD'
100400120314     C**                 Z-ADD     1             I
100500120314     C**   D92NAR        LOOKUP    NAR(I)                                 55
100600120314     C**                 IF        %FOUND
100700120314     C**                 Z-ADD     NARLAD(I)     TASLNA                         *LINEA ARRIVO
100800120314     C**                 ENDIF
100900120314     C**                 ENDIF
101000120314     c
101100120314     c                   if        i10_idDDT='1'
101200120314     C                   eval      tasddt='S'
101300120314     c                   endif
101400120314      *
101500120314     C                   CALL      'TNSF20R'
101600120314     C                   PARM                    KPJBA
101700120314     C                   PARM                    DTAS
101800120314     C                   PARM                    DTASV
101900120314     c
102000120314      /FREE
102100120314
102200120314       // CALCOLA IL TOTALE FATTURA
102300120314 1     if taserr<>*blanks   ;
102400120314         Werror= ESITO_ERROR;
102500120314         clear messag ;
102600120314         messag =      rtvMsgLang('TIS0711':lingua)         ;
102700120314         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
102800120314 x1    else   ;
102900120314
103000120321       // Totale imponibile
103100120321       //  non lo posso passare a zero
103200120321
103300120314        eval(h) o10_idtotimp= tasimv  ;
103400120321        if o10_idtotimp=0   ;
103500120321           o10_idtotimp=0,01  ;
103600120321        endif  ;
103700120314
103800120314       // Descrizione tipo PORTO
103900120314       if tipoporto='F'   ;
104000120314       o10_descrporto='FRANCO   '   ;
104100120314       else  ;
104200120314       o10_descrporto='ASSEGNATO'   ;
104300120314       endif  ;
104400120314
104500120314       // Descrizione linea di partenza
104600120314       o10_idlnp=taslnp           ;
104700120314
104800120314       chain taslnp     azorg01l  ;
104900120314  2    if        %found(azorg01l) ;
105000120314         o10_descrlnp=orgdes      ;
105100120314  2    endif  ;
105200120314
105300120314  2    if i10_idnzm<>*blanks    ;
105400120314        clear tblkey  ;
105500120314  3      if i10_idnzm<>'IT'  ;
105600120314          tblkey=i10_idnzm  ;
105700120314  3      endif  ;
105800120314
105900120314       chain (klin :'15':tblkey)   tabel00f ;
106000120314 3     if %found(tabel00f)  ;
106100120323        o10_descrnzm   =tbluni  ;
106200120314 3     endif   ;
106300120314
106400120314 2     endif  ;
106500120314
106600120314        clear tblkey  ;
106700120314  2      if i10_idnzd<>'IT'  ;
106800120314          tblkey=i10_idnzd  ;
106900120314  2      endif  ;
107000120314
107100120314       chain (klin :'15':tblkey)   tabel00f ;
107200120314 2     if %found(tabel00f)  ;
107300120323        o10_descrnzd   =tbluni  ;
107400120314 2     endif   ;
107500120314
107600120314
107700120314 1     endif  ;
107800120314
107900120314       ENDSR   ;
108000120314
108100120314      /END-FREE
