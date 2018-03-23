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
002200120315      // - Ricerca unificante padre
002300120315     d TIBS10ds      e ds                  inz
002400120315     d  skc                   21   5520    dim(500)
002500120301     d
002600120306     D tis7st10i     E DS                  prefix(i10_) INZ(*EXTDFT)
002700120306     D tis7st10o     E DS                  prefix(o10_) INZ(*EXTDFT)
002800120323     d
002900120323     d fnlv55ds      e ds                  inz
003000120308
003100120314     D* DS "TNSF20R" - TASSAZIONE SINGOLA BOLLA
003200120314     D DTAS01        E DS
003300120314     D DTAS          E DS
003400120314     D DTASV         E DS
003500120314     D  VSV                    1     20    DIM(20)                              SIGLA  VARIE
003600120314     D  VAR                   21    140P 3 DIM(20)
003700120302
003800120301     D rqsOpCode       S             10A
003900120301     D rqsData         S           9999A
004000120301     D rqsDataLen      S              4P 0
004100120301     D rpyOpCode       S             10A
004200120301     D rpyData         S           9999A
004300120301     D rpyDataLen      S              4P 0
004400120301     d
004500120301     d Datasys         s               d   datfmt(*iso) inz(*sys)
004600120301     d Dataiso         s               d   datfmt(*iso)
004700120301     d Dataeur         s               d   datfmt(*eur)
004800120301     d Datarif         s              8  0
004900120301     d codut           s              1  0
005000120301     d XX              s              3  0
005100120301     d werror          s              1  0
005200120305     d klin            s              1  0
005300120315     d wksu            s              8  0
005400120315     d wksc            s             11
005500120315     d Indx            s              4  0
005600120306     d Tipoporto       s              1
005700120315     d Wtipoleg        s              2
005800120309     d Werr            s              1
005900120323     d Wtipo           s              1
006000120315     d Ksun            s              9
006100120312     d messag          s            250
006200120307     d LINGUA          s                   like(I10_LINISO2)
006300120309     d wloc            s                   like(I10_idlom)
006400120309     d wcap            s                   like(I10_idcam)
006500120309     d wnar            s                   like(I10_idnzm)
006600120314     d Mitcts          s                   like(o95cts)
006700120314     d Mitiso          s                   like(o95iso)
006800120314     d Mitlnp          s                   like(o95lna)
006900120323     d Wttc            s              5
007000120323     d WttcCAP         s              5
007100120323     d C               s              3  0
007200120327     d DueCTR          s              2  0
007300120301     ***************************************************************************
007400120306     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
007500120306     D  msgId                         7A   CONST
007600120306     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
007700120306     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
007800120306     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
007900120306     D                                     VARYING
008000120306     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
008100120306     D                                     VARYING
008200120306     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
008300120306     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
008400120306     ***************************************************************************
008500120301     ** Costanti.
008600120301     ***************************************************************************
008700120301     D ESITO_ERROR...
008800120301     D                 C                   -1
008900120301     D ESITO_OK...
009000120301     D                 C                   0
009100120301     D QTEMP...
009200120301     D                 C                   'QTEMP'
009300120301     D RPYOPCODE_DONE...
009400120315     D                 C                   'DONE '
009500120301     D RPYOPCODE_ERROR...
009600120301     D                 C                   'ERROR'
009700120306     D RQSOPCODE_GETTAMCALC...
009800120306     D                 C                   'GETTAMCALC'
009900120301
010000120309      *---------------------------------------------------------------*
010100120309      /COPY GAITRASRC/SRCPROTOPR,TRUL73R
010200120315      /COPY GAITRASRC/SRCPROTOPR,tibs10r
010300120323      /COPY GAITRASRC/SRCPROTOPR,fnlv55r
010400120309      *---------------------------------------------------------------*
010500000000     C     *ENTRY        PLIST
010600120301     C                   PARM                    rqsOpCode
010700120301     C                   PARM                    rqsData
010800120301     C                   PARM                    rqsDataLen
010900120301     C                   PARM                    rpyOpCode
011000120301     C                   PARM                    rpyData
011100120301     C                   PARM                    rpyDataLen
011200120301     c
011300120301     c                   clear                   rpyData
011400120301     c                   clear                   rpyDataLen
011500120315     c                   clear                   rpyopcode
011600120301     c
011700120302     c                   eval      rpyopcode=RPYOPCODE_ERROR
011800120302
011900120301     c                   exsr      Contrdati
012000120302     c
012100120306    1c                   if        Werror= ESITO_OK
012200120306     c                   exsr      contrTas
012300120314     c                   endif
012400120306
012500120314    1c                   if        Werror= ESITO_OK
012600120306     c                   exsr      Tassazione
012700120302    1c                   endif
012800120302
012900120302    1c                   if        Werror= ESITO_OK
013000120302     c                   eval      rpyopcode=RPYOPCODE_DONE
013100120302
013200120307     c                   eval      rpyData = tis7st10o
013300120306     c                   eval      rpyDataLen = %SIZE(tis7st10o)
013400120302     c
013500120302     c                   else
013600120320     c****               dump(a)
013700120314     c                   eval      rpyDataLen = %len(%trim(rpydata))
013800120302    1c                   endif
013900900323     C*
014000120302     C                   return
014100120301     c*------------------------------------------------------------------*
014200120301     C*   controlli dati passati
014300120301     c*------------------------------------------------------------------*
014400120301     c     ContrDATI     BEGSR
014500120301     c                   clear                   Werror
014600120315     c                   clear                   Wksu
014700120314     c                   clear                   TipoPorto
014800120301      /FREE
014900120306 1     IF rqsOpCode <> RQSOPCODE_GETTAMCALC     ;
015000120306         WError=  ESITO_ERROR;
015100120306         rpyData = 'rqsOpCode errato o non gestito' ;
015200120306         leavesr   ;
015300120301 1     ENDIF;
015400120301
015500120301
015600120306       reset tis7st10i;
015700120302
015800120309 1     SELECT;
015900120306         WHEN %SUBST(rqsData : 1 : 10) = i10_formato;
016000120306           tis7st10i = %SUBST(rqsData : 1 : rqsDataLen);
016100120302         OTHER;
016200120302           Werror= ESITO_ERROR;
016300120306         rpyData = 'Richiamo errato' ;
016400120306         leavesr   ;
016500120309 1     ENDSL;
016600120302
016700120309 1     monitor  ;
016800120302
016900120315       // Se non presente passo login per otrnare alla videata di
017000120315       //  collegamento
017100120306 2     IF i10_idcliente <= *zeros ;
017200120302         Werror = ESITO_ERROR;
017300120315         rpyopcode='LOGIN' ;
017400120315         clear rpyData  ;
017500120315         leavesr   ;
017600120309 2     endif   ;
017700120309
017800120315       on-error ;
017900120315         Werror = ESITO_ERROR;
018000120315         rpyopcode='LOGIN' ;
018100120315         clear rpyData  ;
018200120315         leavesr   ;
018300120309 1     endmon  ;
018400120315
018500120315 1     monitor  ;
018600120315       wksu=%int(i10_ksu) ;
018700120315
018800120315       on-error ;
018900120315         Werror = ESITO_ERROR;
019000120315         rpyopcode='LOGIN' ;
019100120315         clear rpyData  ;
019200120315         leavesr   ;
019300120315 1     endmon  ;
019400120315
019500120315 1     IF wksu   <= *zeros ;
019600120315         Werror = ESITO_ERROR;
019700120315         rpyopcode='LOGIN' ;
019800120315         clear rpyData  ;
019900120315         leavesr   ;
020000120315 1     endif   ;
020100120315
020200120315 1     monitor  ;
020300120315
020400120315       // Se non presente passo login per otrnare alla videata di
020500120315       //  collegamento
020600120315 2     IF i10_sun <= *zeros ;
020700120315         Werror = ESITO_ERROR;
020800120315         rpyopcode='LOGIN' ;
020900120315         clear rpyData  ;
021000120315         leavesr   ;
021100120315 2     endif   ;
021200120315
021300120315       on-error ;
021400120315         Werror = ESITO_ERROR;
021500120315         rpyopcode='LOGIN' ;
021600120315         clear rpyData  ;
021700120315         leavesr   ;
021800120315 1     endmon  ;
021900120302
022000120309 1     monitor  ;
022100120306 2     IF i10_idcodtar  <  *zeros ;
022200120302         Werror = ESITO_ERROR;
022300120312         clear messag ;
022400120313         messag =  rtvMsgLang('TIS0705':lingua) ;
022500120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
022600120306         leavesr   ;
022700120302 2     ENDIF;
022800120305
022900120302       on-error ;
023000120302           Werror= ESITO_ERROR;
023100120312         clear messag ;
023200120313         messag =  rtvMsgLang('TIS0705':lingua) ;
023300120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
023400120306         leavesr   ;
023500120309 1     endmon  ;
023600120302
023700120309 1     if i10_tipocli<>'M' and
023800120306          i10_tipocli<>'D' and
023900120306          i10_tipocli<>'O'    ;
024000120302           Werror= ESITO_ERROR;
024100120309         rpyData = %trim(rpyData) +
024200120309                   'Non indicato se cliente è Mittente Destinatario +
024300120309                    o Ordinante.<br>'   ;
024400120309 1     endif  ;
024500120306
024600120306       // Imposto tipo porto
024700120309 1     if i10_tipocli<>'M' ;
024800120307       Tipoporto='A'   ;
024900120306       else  ;
025000120307       Tipoporto='F'   ;
025100120309 1     endif  ;
025200120306
025300120306
025400120306       lingua= i10_liniso2        ;
025500120305
025600120305       // Controllo e selezione lingua
025700120309 1     select ;
025800120305
025900120305       when  lingua ='en'   ;
026000120305        klin =2   ;
026100120305       when  lingua ='fr'   ;
026200120305        klin =3   ;
026300120305       when  lingua ='de'   ;
026400120305        klin =4   ;
026500120305       when  lingua ='it' or lingua='  '  ;
026600120305        klin =1   ;
026700120305       other ;
026800120305           Werror= ESITO_ERROR;
026900120309         rpyData = %trim(rpyData) +
027000120312                   'Codice lingua errato.<br>' ;
027100120309 1     endsl  ;
027200120301
027300120314       // Reperisco divisa corrente
027400120314       tbeke1='1              '          ;
027500120314       chain (' ':'GED':tbeke1)  tntbe11l    ;
027600120314       if %found(tntbe11l)    ;
027700120314       dged=tbeuni  ;
027800120314       endif  ;
027900120314
028000120301      /END-FREE
028100120301     c                   ENDSR
028200120302     c*------------------------------------------------------------------*
028300120306     C*   controllo ai fini della tassazione
028400120302     c*------------------------------------------------------------------*
028500120306     c     contrTas      BEGSR
028600120302      /FREE
028700120305
028800120306       reset tis7st10o   ;
028900120306       clear xx ;
029000120309       clear ds3idp ;
029100120314       clear mitlnp  ;
029200120314       clear mitiso  ;
029300120314       clear mitcts  ;
029400120315       clear wtipoleg  ;
029500120323       clear wtipo     ;
029600120323       clear ds5e      ;
029700120327       clear ddfe      ;
029800120309
029900120309 1     if i10_idnetwork='DPD'   ;
030000120309         clear tblkey  ;
030100120309         tblkey='DPD'   ;
030200120309
030300120309         chain (1 :'3I':tblkey)   tabel00f ;
030400120309 2       if %found(tabel00f)  ;
030500120309          ds3idp=tbluni   ;
030600120309 2       endif   ;
030700120309 1     endif   ;
030800120327
030900120327 1     if i10_idnetwork='FED'   ;
031000120327       clear   tbeke1   ;
031100120327       tbeke1='FED'   ;
031200120327       chain (' ':'DFE':tbeke1)  tntbe11l    ;
031300120305
031400120327       if %found(tntbe11l)    ;
031500120327       dDFE=tbeuni  ;
031600120327       endif  ;
031700120327       endif   ;
031800120327
031900120309       // Prima controllo i campi che mi devono arrivare numerici
032000120309
032100120308       // DATA SPEDIZIONE
032200120306
0323001203061      if i10_dtspediz<>*blanks and i10_dtspediz<>*zeros ;
032400120302
0325001203092      monitor  ;
032600120306         dataiso=%date(i10_DTSPEDIZ:  *eur)   ;
032700120302         on-error ;
032800120302           Werror= ESITO_ERROR;
032900120312         rpyData =  rtvMsgLang('TIS0121':lingua) ;
033000120312         rpydata=%trim(rpydata)+'.<br>' ;
033100120309 2     endmon  ;
033200120302
033300120302x1     else    ;
033400120302       dataiso=datasys   ;
033500120302 1     endif     ;
033600120309
033700120309 1     if werror =esito_ok  ;
033800120309         datarif=%dec(dataiso)   ;
033900120309 1     endif  ;
034000120309
034100120309       // LINEA DI PARTENZA
034200120309 1     Monitor  ;
034300120309 2     IF  i10_idlnp  <  *zeros ;
034400120309         Werror = ESITO_ERROR;
034500120312         clear messag ;
034600120313         messag =  rtvMsgLang('TIS0699':lingua) ;
034700120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
034800120309 2     ENDIF;
034900120309
035000120309       on-error ;
035100120309           Werror= ESITO_ERROR;
035200120312         clear messag ;
035300120313         messag =  rtvMsgLang('TIS0699':lingua) ;
035400120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
035500120309 1     endmon  ;
035600120309
035700120309       //  COLLI
035800120309 1     Monitor  ;
035900120309 2     IF  i10_idncl  <= *zeros ;
036000120309         Werror = ESITO_ERROR;
036100120312         clear messag ;
036200120312         messag=   rtvMsgLang('TIS0702':lingua)  ;
036300120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
036400120309 2     ENDIF;
036500120309
036600120309       on-error ;
036700120309           Werror= ESITO_ERROR;
036800120312         clear messag ;
036900120312         messag  = rtvMsgLang('TIS0702':lingua)  ;
037000120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
037100120309 1     endmon  ;
037200120309
037300120309       // PESO
037400120309 1     Monitor  ;
037500120309 2     IF  i10_idpkf  <= *zeros ;
037600120309         Werror = ESITO_ERROR;
037700120312           clear messag ;
037800120313           messag =rtvMsgLang('TIS0703':lingua) ;
037900120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
038000120309 2     ENDIF;
038100120309
038200120309       on-error ;
038300120309           Werror= ESITO_ERROR;
038400120312           clear messag ;
038500120313         messag  = rtvMsgLang('TIS0703':lingua) ;
038600120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
038700120309 1     endmon  ;
038800120309
038900120309       // VOLUME
039000120309 1     Monitor  ;
039100120323 2     //   IF  i10_idvlf  <= *zeros ;
039200120323 2     IF  i10_idvlf  <  *zeros ;
039300120309         Werror = ESITO_ERROR;
039400120312           clear messag ;
039500120313         messag  = rtvMsgLang('TIS0704':lingua) ;
039600120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
039700120309 2     ENDIF;
039800120309
039900120309       on-error ;
040000120309           Werror= ESITO_ERROR;
040100120312           clear messag ;
040200120312         messag  = rtvMsgLang('TIS0704':lingua)+'.<br>' ;
040300120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
040400120309 1     endmon  ;
040500120309
040600120309       // C/ASSEGNO
040700120309 1     Monitor  ;
040800120309 2     IF  i10_idcas  <  *zeros ;
040900120309         Werror = ESITO_ERROR;
041000120312           clear messag ;
041100120312         messag =  rtvMsgLang('TIS0269':lingua)+'.<br>' ;
041200120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
041300120309         WErr='1'  ;
041400120309 2     ENDIF;
041500120309
041600120309       on-error ;
041700120309           Werror= ESITO_ERROR;
041800120312           clear messag ;
041900120312         messag =  rtvMsgLang('TIS0269':lingua)+'.<br>' ;
042000120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
042100120309 1     endmon  ;
042200120309
042300120309       // Importo ASSICURARE
042400120309 1     Monitor  ;
042500120309 2     IF  i10_idias  <  *zeros ;
042600120309         Werror = ESITO_ERROR;
042700120312           clear messag ;
042800120312         messag =  rtvMsgLang('TIS0269':lingua)+'.<br>' ;
042900120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
043000120309 2     ENDIF;
043100120309
043200120309       on-error ;
043300120309           Werror= ESITO_ERROR;
043400120312           clear messag ;
043500120312            messag=rtvMsgLang('TIS0269':lingua)+'.<br>' ;
043600120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
043700120309 1     endmon  ;
043800120309
043900120309       // Quantità da FATTURARE
044000120309 1     Monitor  ;
044100120309 2     IF  i10_idias  <  *zeros ;
044200120309         Werror = ESITO_ERROR;
044300120312           clear messag ;
044400120312         messag =  rtvMsgLang('TIS0269':lingua)+'.<br>' ;
044500120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
044600120309 2     ENDIF;
044700120309
044800120309       on-error ;
044900120309           Werror= ESITO_ERROR;
045000120312           clear messag ;
045100120313         messag =  rtvMsgLang('TIS0269':lingua) ;
045200120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
045300120309 1     endmon  ;
045400120302
045500120309       // Tipo SERVIZIO "C" unico ammesso per export
045600120309
045700120309 1     if tipoporto='F' and i10_idtsp<>'C' and i10_idnetwork<>*blanks   ;
045800120309         Werror = ESITO_ERROR;
045900120312           clear messag ;
046000120313         messag=   rtvMsgLang('TIS0706':lingua) ;
046100120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
046200120309 1     ENDIF;
046300120313
046400120313       // se non è in tabella --> errore
046500120313       clear tblkey  ;
046600120314       tblkey=i10_idtsp   ;
046700120313
046800120313       chain (klin:'5E':tblkey) tabel00f   ;
046900120313 1     if not %found(tabel00f)  ;
047000120314         Werror = ESITO_ERROR;
047100120314         clear messag ;
047200120313         messag=   rtvMsgLang('TIS0551':lingua) ;
047300120313         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
047400120323         else  ;
047500120323         ds5e=tbluni   ;
047600120313 1     endif  ;
047700120423
047800120423       // Bolle Fedex: impossibile spedizione in assegnato
047900120423 1     if i10_idnetwork='FED' and tipoporto='A' ;
048000120423         Werror = ESITO_ERROR;
048100120423         clear messag ;
048200120423         messag=   rtvMsgLang('TIS0717':lingua) ;
048300120423         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
048400120423       endif  ;
048500120309
048600120313       // per questi ERRORI  devo uscire subito dal pgm
048700120309 1     if Werror = ESITO_ERROR;
048800120309         leavesr  ;
048900120309 1     endif  ;
049000120315
049100120315         // Verifico congruenza dei codici del cliente che richiede la tassazione
049200120315
049300120315         ksun=%editc(i10_sun:'X')  ;
049400120315         chain (Ksun:'TT' ) tivss01l   ;
049500120315 1       if %found( tivss01l) and datarif>=vssdde and
049600120315            datarif<=vssdsc    ;
049700120315            Wtipoleg=vsstle  ;
049800120315         else  ;
049900120315          Werror = ESITO_ERROR;
050000120315          rpyopcode='LOGIN' ;
050100120315          clear rpyData  ;
050200120315          leavesr   ;
050300120315 1       endif     ;
050400120315
050500120315       // Prendo codice unificante e carico cod collegati figli
050600120315           clear tibs10ds ;
050700120315           d10tle=wtipoleg  ;
050800120315           d10paf='F'    ;
050900120315           d10cod=wksu   ;
051000120315           callp TIBS10R (tibs10ds)   ;
051100120315
051200120315  1        if d10err<>*blanks   ;
051300120315           // Se errore verifico se è figlio
051400120315           clear tibs10ds ;
051500120315           d10tle=wtipoleg    ;
051600120315           d10paf='P'    ;
051700120315           d10cod=wksu;
051800120315           callp TIBS10R (tibs10ds)   ;
051900120315
052000120315  2        if d10err<>*blanks   ;
052100120315             // Imposto nella skiera e come padre solo se stesso
052200120315             skc(1)='000'+%editc(wksu:'X');
052300120315             d10cop=wksu                  ;
052400120315  2        endif                ;
052500120315  1        endif ;
052600120315
052700120315           wksc='0000'+%editc(i10_idcliente:'X');
052800120315           Indx=%lookup(wksc:skc)   ;
052900120315  1        if Indx=0 ;
053000120315            Werror = ESITO_ERROR;
053100120315            rpyopcode='LOGIN' ;
053200120315            clear rpyData  ;
053300120315            leavesr   ;
053400120315  1        endif  ;
053500120308
053600120309       // CONTROLLO LIMITI DI PESO/COLLI/VOLUME
053700120309       clear trul73ds   ;
053800120309       i73tsp=i10_idtsp   ;
053900120309       i73psk=i10_idpkf  ;
054000120309       i73vmc=i10_idvlf ;
054100120309       i73ncl=i10_idncl ;
054200120309       callp TRUL73R  (trul73ds)   ;
054300120309
054400120309       // errore colli
054500120309       if  o73fclm='1'       ;
054600120309         Werror = ESITO_ERROR;
054700120314         clear messag ;
054800120326         messag =  rtvMsgLang('TIS0707':lingua:§5edes) ;
054900120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
055000120309 1     ENDIF;
055100120309       // Errore peso
055200120309       if  o73fkgm='1'       ;
055300120309         Werror = ESITO_ERROR;
055400120312           clear messag ;
055500120326         messag =  rtvMsgLang('TIS0708':lingua:§5edes) ;
055600120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
055700120309 1     ENDIF;
055800120309       // Errore volume
055900120309       if  o73fmcm='1'       ;
056000120309         Werror = ESITO_ERROR;
056100120312           clear messag ;
056200120326         messag =  rtvMsgLang('TIS0709':lingua:§5edes) ;
056300120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
056400120309 1     ENDIF;
056500120309
056600120309       //  CONTROLLI COLLI E PESO PER DPD
056700120309  1    IF I10_idNETWORK='DPD'    ;
056800120309
056900120314  2      IF I10_IDncl<>1  or i10_idpkf>§3ipkd  ;
057000120309         Werror = ESITO_ERROR;
057100120312           clear messag ;
057200120323         messag =  rtvMsgLang('TIS0710':lingua:%editc(§3ipkd:'4'))  ;
057300120323         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
057400120309  2      endif  ;
057500120309
057600120309  1    ENDIF   ;
057700120309
057800120309       // secondo gruppo di errori che prevede l'uscita dal pgm
057900120309 1     if Werror = ESITO_ERROR;
058000120309         leavesr  ;
058100120309 1     endif  ;
058200120309
058300120327       //  CONTROLLO PESO PER FEDEX documenti
058400120327  1    IF I10_idNETWORK='FED'    ;
058500120327         duectr=%int(%subst(%editc(i10_idcodtar :'X'):2:2))  ;
058600120327  2      if i10_idpkf> §DfePkgD and dueCtr >= §DfeDalD
058700120327            and dueCtr <= §DfeAlD     ;
058800120327         Werror = ESITO_ERROR;
058900120327           clear messag ;
059000120327         messag =  rtvMsgLang('TIS0716':lingua:%editc(§dfepkgd:'4'))  ;
059100120327         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
059200120327  2      endif  ;
059300120327  1    endif  ;
059400120327
059500120309
059600120308       // LINEA DI PARTENZA
059700120308
059800120308       // Per i franchi se manca la linea di partenza --> errore  ;
059900120308
060000120309 1     IF Tipoporto='F' and i10_idlnp  <= *zeros ;
060100120308         Werror = ESITO_ERROR;
060200120312           clear messag ;
060300120313         messag =  rtvMsgLang('TIS0699':lingua) ;
060400120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
060500120309 1     ENDIF;
060600120308
060700120309 1       IF i10_idlnp  >  *zeros  ;
060800120308           chain  i10_idlnp  azorg01l ;
060900120309 2         if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A');
061000120308             Werror= ESITO_ERROR;
061100120312           clear messag ;
061200120313         messag =      rtvMsgLang('TIS0699':lingua) ;
061300120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
061400120309 2         endif  ;
061500120309 1       endif  ;
061600120301
061700120308       // MITTENTE: nazione/cap/località solo per asssegnati
061800120308        WERR='0'  ;
061900120313        clear   tisi97ds     ;
062000120315        clear   fnlv12ds  ;
062100120314        clear  wnar ;
062200120314        clear  wcap ;
062300120314        clear  wloc ;
062400120323        wtipo='M'  ;
062500120309
062600120309 1     if tipoporto='A' and i10_idcam=*blanks  ;
062700120308         Werror = ESITO_ERROR;
062800120312           clear messag ;
062900120313         messag = rtvMsgLang('TIS0237':lingua) ;
063000120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
063100120308 1     ENDIF;
063200120308
063300120309 1     if i10_idcam<>*blanks   ;
063400120312         if i10_idnzm='IT'  ;
063500120312         clear wnar  ;
063600120312         else  ;
063700120309         wnar=i10_idnzm   ;
063800120312         endif  ;
063900120309         wcap=i10_idcam  ;
064000120309         exsr   CHKcap   ;
064100120309
064200120309 2       if Werr='1'   ;
064300120314           Werror= ESITO_ERROR;
064400120312           clear messag ;
064500120319         messag =      rtvMsgLang('TIS0700':lingua:i10_idcam) ;
064600120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
064700120309         Werr='1'    ;
064800120309 2       endif   ;
064900120309
065000120309
065100120309 2      if i10_idlom<>*blanks  and werr='0'  ;
065200120312           if i10_idnzm='IT'  ;
065300120312            clear wnar  ;
065400120312           else  ;
065500120309           wnar=i10_idnzm   ;
065600120312           endif   ;
065700120309           wcap=i10_idcam  ;
065800120309           wloc=i10_idlom  ;
065900120309           exsr   CHKclp   ;
066000120309
066100120309 3       if Werr='1'   ;
066200120314           Werror= ESITO_ERROR;
066300120312           clear messag ;
066400120319         messag =      rtvMsgLang('TIS0701':lingua:wloc) ;
066500120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
066600120309 3       endif   ;
066700120309 2       endif   ;
066800120315
066900120315
067000120315           // Se presente network, controllo se utilizzabile per ritiri
067100120315 2           if werr='0' and i10_idnetwork<>*blanks   ;
067200120315              i97ntw =i10_idnetwork  ;
067300120315              i12lang=lingua  ;
067400120315      /END-FREE
067500120315
067600120315     C                   CALL      'FNLV12R'
067700120315     C                   PARM                    fnlv12ds
067800120315     C                   PARM                    tisi95ds
067900120315     C                   PARM                    tisi97ds
068000120315
068100120315      /FREE
068200120315 3           if o12err<>*blanks   ;
068300120315              Werror= ESITO_ERROR;
068400120315              rpyData = %trim(rpyData)  + %trim(o12msg)+'.<br>' ;
068500120315              WErr='1'  ;
068600120315 3           endif   ;
068700120315 2           endif   ;
068800120315
068900120315
069000120313        // Se non ci sono errori, memorizzo linea di partenza se manca cod tas
069100120313        //  e flag inoltro
069200120313 2          if werr='0'   ;
069300120314          mitcts=o95cts  ;
069400120314          mitiso=o95iso  ;
069500120314          mitlnp=o95lna  ;
069600120313 2          endif   ;
069700120313 1       endif   ;
069800120312
069900120312       // DESTINATARIO: nazione/cap/località solo per asssegnati
070000120312        WERR='0'  ;
070100120315        clear   tisi97ds     ;
070200120314        clear  wnar ;
070300120314        clear  wcap ;
070400120314        clear  wloc ;
070500120323        wtipo='D'  ;
070600120313
070700120312         if i10_idnzd='IT'  ;
070800120312         clear wnar  ;
070900120312         else  ;
071000120312         wnar=i10_idnzd   ;
071100120312         endif  ;
071200120312         wcap=i10_idcad  ;
071300120313
071400120312         exsr   CHKcap   ;
071500120312
071600120312 1       if Werr='1'   ;
071700120312             Werror= ESITO_ERROR;
071800120312           clear messag ;
071900120319         messag =      rtvMsgLang('TIS0700':lingua:i10_idcad) ;
072000120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
072100120312         Werr='1'    ;
072200120312 1       endif   ;
072300120312
072400120312
072500120312 1      if i10_idlod<>*blanks  and werr='0'  ;
072600120312           if i10_idnzd='IT'  ;
072700120312            clear wnar  ;
072800120312           else  ;
072900120312           wnar=i10_idnzd   ;
073000120312           endif   ;
073100120312           wcap=i10_idcad  ;
073200120312           wloc=i10_idlod  ;
073300120313
073400120312           exsr   CHKclp   ;
073500120312
073600120312 2       if Werr='1'   ;
073700120314           Werror= ESITO_ERROR;
073800120312           clear messag ;
073900120319         messag =      rtvMsgLang('TIS0701':lingua:wloc)         ;
074000120312         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
074100120312 2       endif   ;
074200120312 1       endif   ;
074300120313
074400120313       // Se presente network, controllo se utilizzabile
074500120315 1       if werr='0' and i10_idnetwork<>*blanks   ;
074600120313
074700120314          i97ntw =i10_idnetwork  ;
074800120314          exsr chkcap   ;
074900120314
075000120830 2        if werr='1'         ;
075100120314            Werror= ESITO_ERROR;
075200120314            dcppflo=o97flo   ;
075300120314            clear messag ;
075400120314            messag =      o13msg          ;
075500120314            rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
075600120314            if §cppall='S'   ;
075700120314            endif  ;
075800120315 2       endif  ;
075900120315 1       endif  ;
076000120323
076100120323       // controllo tipo servizio se ci sono limitazioni di codici tempo
076200120323       clear tblkey  ;
076300120323       clear  ds5e  ;
076400120323       tblkey=i10_idtsp  ;
076500120323
076600120323       chain (klin :'5E':tblkey)   tabel00f ;
076700120323 1     if %found(tabel00f)  ;
076800120323        ds5e=tbluni   ;
076900120323        o10_descrtsp= §5edes  ;
077000120323 1     endif   ;
077100120323
077200120323      /END-FREE
077300120323     c
077400120323    1c                   if        werr='0' and §5ettc<>*blanks
077500120323     c* controllo posizione per posizione escudendo le "x"
077600120323     c                   eval      Wttc=§5ettc
077700120323     c                   eval      WttcCAP=%editc(o95ttc:'X')
077800120323     c                   z-add     1             c
077900120323    2c                   do        5             c
078000120323    3c                   if        %subst(wttc:c:1)<>'x'
078100120323     c                             and
078200120323     c                             %subst(wttc:c:1)<>%subst(wttcCAP:c:1)
078300120323     c                   eval      Werror= ESITO_ERROR
078400120323     c                   clear                   messag
078500120323     c                   eval      messag =
078600120323     c                                   rtvMsgLang('TIS0715':LINGUA:§5edes)
078700120323     c                   eval      rpyData=%trim(rpyData)+%trim(messag)+'.<br>'
078800120323    3c                   endif
078900120323    2c                   enddo
079000120323    1c                   endif
079100120323     c
079200120323     c                   ENDSR
079300120302
079400120323      /FREE
079500120309       //-----------------------------------------------------------------*
079600120309       //  controllo CAP
079700120309       //-----------------------------------------------------------------*
079800120309       BEGSR   CHKcap     ;
079900120313
080000120313       exsr     CAllFNLV13 ;
080100120313
080200120309       if o95err<>*blanks   or
080300120309          (o13err<>*blanks and o13rop=*blanks)   ;
080400120309         WErr='1'  ;
080500120309       endif  ;
080600120309
080700120309       ENDSR    ;
080800120309       //-----------------------------------------------------------------*
080900120309       //  controllo CAP/LOCALITà
081000120309       //-----------------------------------------------------------------*
081100120309       BEGSR   CHKclp     ;
081200120313
081300120313       exsr     CAllFNLV13 ;
081400120309
081500120309       if o95err<>*blanks   or
081600120312          (o13err<>*blanks and o13rop=*blanks)   or
081700120313          (o95lia = '1') or
081800120802          (wnar= *blanks and wloc<>*blanks and o95lid<'3') ;
081900120309         WErr='1'  ;
082000120309       endif  ;
082100120309
082200120309       ENDSR    ;
082300120309
082400120313       //-----------------------------------------------------------------*
082500120313       //  Richiamo pgm fnlv13r
082600120313       //-----------------------------------------------------------------*
082700120314       BEGSR CallFNLV13  ;
082800120314
082900120313       clear fnlv13ds  ;
083000120313       clear tisi95ds  ;
083100120313       i95tcn='3'  ;
083200120314       i95nar=wnar       ;
083300120802
083400120802        // CHIODO per IRLANDA
083500120802       if wnar='IRL'     ;
083600120802        i95cap='EIRE'  ;
083700120802       else  ;
083800120802        i95cap=wcap       ;
083900120802        endif  ;
084000120314       i95loc=wloc       ;
084100120313       i95dat=datarif  ;
084200120313       i95lkg=i10_idpkf  ;
084300120313       i95lmc=i10_idvlf   ;
084400120313       i95tsp=i10_idtsp  ;
084500120313       i95tpo=tipoporto ;
084600120323       if    i10_idffd ='1'   ;
084700120323         i95ffd='S'       ;
084800120323       endif  ;
084900120323
085000120323       // solo per destinatario
085100120323 1     if wtipo='D'  ;
085200120323
085300120323       // calcolo terminal di partenza ddella linea di partenza
085400120323       clear fnlv55ds  ;
085500120323 2      if i10_idlnp>0    ;
085600120323         d55lin=i10_idlnp  ;
085700120323 x2      else  ;
085800120323
085900120323 3       if mitlnp>0   ;
086000120323         d55lin=mitlnp   ;
086100120323 3      endif  ;
086200120323 2      endif  ;
086300120323
086400120323 2      if   d55lin>0  ;
086500120323        i95fre='S'        ;
086600120323        d55tpt='P'  ;
086700120323        d55drf=datarif   ;
086800120323        callp FNLV55R  (fnlv55ds)  ;
086900120323        i95tfp=d55tfp  ;
087000120323 2      endif  ;
087100120323 1      endif  ;
087200120313
087300120313       if i10_idnetwork='FED'   ;
087400120313         I95fi1='S'  ;
087500120313       endif    ;
087600120313       if i10_idnetwork='DPD'   ;
087700120313         I95fi1='D'  ;
087800120313       endif    ;
087900120313
088000120313       // C/Asssegno
088100120313       if i10_idcas > *zeros   ;
088200120313        i95fca='S'    ;
088300120313       endif   ;
088400120313
088500120313      /END-FREE
088600120313
088700120313     C                   CALL      'FNLV13R'
088800120313     C                   PARM                    KPJBA
088900120313     C                   PARM                    fnlv13ds
089000120313     C                   PARM                    tisi95ds
089100120313     C                   PARM      ' '           flgbac            1
089200120313     C                   PARM                    tisi97ds
089300120322     C                   PARM                    lingua
089400120313     c
089500120313     c                   ENDSR
089600120307     c*------------------------------------------------------------------*
089700120307     C*   Tassazione
089800120307     c*------------------------------------------------------------------*
089900120307     c     Tassazione    BEGSR
090000120314     C                   CLEAR                   DTAS                           *PULISCE DS DI LANCI
090100120314     C                   CLEAR                   DTASV
090200120314     C                   MOVEL     'L'           TASTLA                         *ELABORA E CHIUDE
090300120314     C                   MOVEL     i10_idcliente TASKSC
090400120314     C                   MOVEL     i10_idcodtar  TASCTR
090500120314     C*
090600120314     C* CALCOLA I SINGOLI ELEMENTI DI TASSAZIONE
090700120314IF  3C     Tipoporto     IFEQ      'F'                                          *FRANCO->DATI DESTIN
090800120314     C                   MOVEL     'F1'          TASTBL                         *TIPO BOLLA
090900120314X   3C                   ELSE                                                   *ASSEGNATO->DATI MIT
091000120314     C                   MOVEL     'A2'          TASTBL                         *TIPO BOLLA
091100120314     C                   MOVEL     Mitcts        TASMCT                         *CODICE TASSAZIONE
091200120314     C                   MOVEL     Mitiso        TASFAP                         *FLAG INOLTRO
091300120314E   3C                   ENDIF
091400120314     c* Se consegna a magazzino, flag anteporto='C'
091500120314     c                   if        i10_idRicMag='1'
091600120314     c                   eval      tasfap='C'
091700120314     c                   eval      tasfdn='S'
091800120314     c                   endif
091900120314     c
092000120314     C                   Z-ADD     i10_idncl     TASNCL                         *COLLI
092100120314     C                   Z-ADD     i10_idpkf     TASPKF                         *PESO
092200120314     C                   Z-ADD     i10_idvlf     TASVLF                         *VOLUME
092300120314     C                   Z-ADD     i10_idcas     TASCAS                         *IMPORTO C/ASSEGNO
092400120314     C                   MOVEL     §GEDCr        TASvca                         *DIVISA NON LA SO
092500120314     c* Lasciamo impostato *blanks come "BRT"
092600120314     C*????              MOVEL     D92TIC        TASTIC                         *INCASSO C/ASSENGO
092700120314     C                   MOVEL     §GEDCr        TASvas                         *DIVISA NON LA SO
092800120314     C                   Z-ADD     i10_idias     TASIAS                         *ASSICURAZIONE
092900120314     C                   Z-ADD     i10_idqta     TASqft                         *ASSICURAZIONE
093000120314     C                   Z-ADD     datarif       TASDSP                         *DATA SPEDIZIONE
093100120314     C                   Z-ADD     datarif       TASDCT                         *DATA CAR TAR CAR
093200120314     c                   if        i10_idnzd<>'IT'
093300120314     C                   MOVEL     i10_idnzd     TASNZD                         *NAZIONE DESTINO
093400120314     c                   endif
093500120314     c                   if        i10_idnzm<>'IT'
093600120314     C                   MOVEL     i10_idnzm     TASNZM                         *NAZIONE MITTENTE
093700120314     c                   endif
093800120314     C                   MOVEL     i10_idcad     TASCAD                         *CAP DESTINATARIO
093900120314     c
094000120314     C                   MOVEL     O95CTS        TASCTS                         *CODICE TASSAZIONE
094100120314     C                   MOVEL     O95ISO        TASFIN                         *FLAG INOLTRO
094200120314     C                   MOVEL     §GEDCN        TASDIV                         *DIVISA NON LA SO
094300120314     c* Se fermo deposito flag inoltro = 'C'
094400120314     c                   if        i10_idffd ='1'
094500120314     c                   movel     'C'           tasfin
094600120314     c                   endif
094700120314     C                   MOVEL     i10_idtsp     TASTSP                         *TIPO SERVIZIO
094800120314     c                   if        i10_idlnp>*zeros
094900120314     C                   Z-ADD     i10_idlnp     TASLNP                         *LINEA PARTENZA
095000120314     c                   else
095100120314     c                   z-add     mitlnp        taslnp
095200120314     c                   endif
095300120314     C                   Z-ADD     O95LNA        TASLNA                         *LINEA ARRIVO
095400120314     C* IMPOSTO LA LINEA DI ARRIVA IN RELAZIONE AL NETWROK
095500120314     C**   D92NTW        IFEQ      'DPD'
095600120314     C**                 Z-ADD     1             I
095700120314     C**   D92NAR        LOOKUP    NAR(I)                                 55
095800120314     C**                 IF        %FOUND
095900120314     C**                 Z-ADD     NARLAD(I)     TASLNA                         *LINEA ARRIVO
096000120314     C**                 ENDIF
096100120314     C**                 ENDIF
096200120314     c
096300120314     c                   if        i10_idDDT='1'
096400120314     C                   eval      tasddt='S'
096500120314     c                   endif
096600120314      *
096700120314     C                   CALL      'TNSF20R'
096800120314     C                   PARM                    KPJBA
096900120314     C                   PARM                    DTAS
097000120314     C                   PARM                    DTASV
097100120314     c
097200120314      /FREE
097300120314
097400120314       // CALCOLA IL TOTALE FATTURA
097500120314 1     if taserr<>*blanks   ;
097600120314         Werror= ESITO_ERROR;
097700120314         clear messag ;
097800120314         messag =      rtvMsgLang('TIS0711':lingua)         ;
097900120314         rpyData = %trim(rpyData)  + %trim(messag)+'.<br>' ;
098000120314 x1    else   ;
098100120314
098200120321       // Totale imponibile
098300120321       //  non lo posso passare a zero
098400120321
098500120314        eval(h) o10_idtotimp= tasimv  ;
098600120321        if o10_idtotimp=0   ;
098700120321           o10_idtotimp=0,01  ;
098800120321        endif  ;
098900120314
099000120314       // Descrizione tipo PORTO
099100120314       if tipoporto='F'   ;
099200120314       o10_descrporto='FRANCO   '   ;
099300120314       else  ;
099400120314       o10_descrporto='ASSEGNATO'   ;
099500120314       endif  ;
099600120314
099700120314       // Descrizione linea di partenza
099800120314       o10_idlnp=taslnp           ;
099900120314
100000120314       chain taslnp     azorg01l  ;
100100120314  2    if        %found(azorg01l) ;
100200120314         o10_descrlnp=orgdes      ;
100300120314  2    endif  ;
100400120314
100500120314  2    if i10_idnzm<>*blanks    ;
100600120314        clear tblkey  ;
100700120314  3      if i10_idnzm<>'IT'  ;
100800120314          tblkey=i10_idnzm  ;
100900120314  3      endif  ;
101000120314
101100120314       chain (klin :'15':tblkey)   tabel00f ;
101200120314 3     if %found(tabel00f)  ;
101300120323        o10_descrnzm   =tbluni  ;
101400120314 3     endif   ;
101500120314
101600120314 2     endif  ;
101700120314
101800120314        clear tblkey  ;
101900120314  2      if i10_idnzd<>'IT'  ;
102000120314          tblkey=i10_idnzd  ;
102100120314  2      endif  ;
102200120314
102300120314       chain (klin :'15':tblkey)   tabel00f ;
102400120314 2     if %found(tabel00f)  ;
102500120323        o10_descrnzd   =tbluni  ;
102600120314 2     endif   ;
102700120314
102800120314
102900120314 1     endif  ;
103000120314
103100120314       ENDSR   ;
103200120314
103300120314      /END-FREE
