000100081008     ***************************************************************************
000200081008     **
000400171109     ** scrive FIAPD40f record "TEL" se i dati sono corretti
000500081223     **
001100081008     **
001200081008     ***************************************************************************
001201150908     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('QC2LE')
001203171117     H BNDDIR('TRUL') BNDDIR('TIBS':'UBBNDDIR')
001400081030
001500081030     ***************************************************************************
001600081030     **
001700081030     ** Definizioni costanti.
001800081030     **
001900081030     ***************************************************************************
001901171117     Ffiapd01l  iF   E           K DISK    usropn
001902171117     Ffiapd41l  uF a E           K DISK    usropn commit
001903171117     d fiapd4ds      e ds                  extname(fiapd40f)
001905171201     d fiapdds       e DS                  extname(fiapd00f)
001907171201     d fiapd4b       e DS                  extname(fiapd40f) prefix(B)
001910171109     D dapd4tel      E DS                  inz
001911171201     D dapd4for      E DS                  inz
001913171201     D fnlv36ds      E DS                  inz
001914171117     d V1InCellOp      s              1    inz('0')
001916171221     D mailto1         s             50    inz('CEDALERT@BRT.IT')
001917171221     D mailto2         s             50    inz('BARBARA.ARMAROLI@BRT.IT')
001918180207     D mailto3         s             50    inz('MICHELE.MALIPIERO@BRT.IT')
001919171201     D EsitoCel        s              4s 0
001920150909     D psds           SDS
001921150909     D  jobName              244    253A
001922150909     D  jobUser              254    263A
001923150909     D  jobNumber            264    269A
001924171213     d testcel         ds
001925171213     d cel                     1     16    dim(16)
001926171213     d num                     1     16
001927171213     d apd4cf          s             16
001928171211     d apd4ksc         s                   like(§apd4ksc)
001929171109     d apd4soc         s                   like(§apd4soc)
001930171109     d apd4pi          s                   like(§apd4pi)
001931171109     d apd4cel         s                   like(§apd4cel)
001932150909     D REPLACE...
001933150909     D                 C                   'R'
001934151022     D msg             S            300
001935151022     D $msg            S            100
001936150909     D $campo          S             30
001937150909     D $rrn            S              9  0
001938150907     D Esito           S              1
001939150907     D GesMsg          S              1
001940150907     D Output          S           4000
001941150907     D LenOut          S              9B 0
001942150907     D StrutturaO      S             10
001943150907     D Refresh         S              1
002000081030     D ESITO_ERROR...
002100081030     D                 C                   -1
002200081030     D ESITO_OK...
002300081030     D                 C                   0
002400081030     D ESITO_WARNING...
002500081030     D                 C                   1
003000081223
003100081030     ***************************************************************************
003200081030     **
003300081030     ** Definizioni procedure esterne.
003400081030     **
003500081030     ***************************************************************************
003501150908     D/COPY GAITRASRC/SRCPROTOPR,SYSTEM
003502150909     D/COPY GAITRASRC/SRCPROTOPR,TRULEMAIL
003504171201     D/copy gaitrasrc/srcProtoPi,UBCHKCEL
003506171201     D/copy gaitrasrc/srcProtoPr,UBCHKCEL
003700081030
003800081015     ***************************************************************************
003900081015     **
004000081015     ** Dichiarazione variabili.
004100081015     **
004200081015     ***************************************************************************
004300081015     D rpyEsito        S             10I 0
004400081015     D rpyOpCode       S             10A
004500081015     D rpyStatus       S             10I 0
004600081015     D rqsOpCode       S             10A
004601171201     D tivlrds       E DS                  extname(tivlr00f)
004800081015
004900081015     ***************************************************************************
005000081015     **
005100081015     ** Dichiarazione parametri.
005200081015     **
005300081015     ***************************************************************************
005400081015     C     *ENTRY        PLIST
005500081015     C                   PARM                    rqsOpCode
005600081015     C                   PARM                    rpyEsito
005700081015     C                   PARM                    rpyOpCode
005800081015     C                   PARM                    rpyStatus
005801171201     C                   PARM                    tivlrds
005835150907      *
005841171109     C     Kapd4         KLIST                                                  *
005842171109     C                   KFLD                    APD4TIPA                       *
005843171109     C                   KFLD                    APD4PDR                        *
005844171109     C                   KFLD                    APD4TRD                        *
005845171201     C     Kapd          KLIST                                                  *
005846171201     C                   KFLD                    APD4TIPA                       *
005847171201     C                   KFLD                    APD4PDR                        *
005848171109     c                   eval      apd4tipa = 'A'
005900081015
006000081015     ***************************************************************************
006100081015     **
006200081015     ** Main.
006300081015     **
006400081015     ***************************************************************************
006500081015      /FREE
006600081015
006700081015       *INLR = *ON;
006800081015
006802151023         EXEC SQL
006803151023           SET TRANSACTION
006804151023             ISOLATION LEVEL READ UNCOMMITTED, READ WRITE ;
006805151023
006900081015       CLEAR rpyEsito;
007000081015       CLEAR rpyOpCode;
007100081015       CLEAR rpyStatus;
007101150907
007102171117       open fiapd01l;
007103171117       open fiapd41l;
007104151023
007105150907         // leggo l'archivio in QTEMP
007106150907       EXEC SQL
007107150907         DECLARE CLIENTI NO SCROLL CURSOR FOR
007108150915           SELECT
007109171109                  autista,
007110171109                  cf,
007111171109                  pi,
007113171109                  fornitore,
007114171201                  societa,
007115171109                  telefono,
007116171201                  rrn(tl_db2)
007117171109           FROM tl_db2;
007118150907       EXEC SQL
007119150907         OPEN CLIENTI;
007120150907
007121150907       IF sqlCode < 0;
007122150907         rpyEsito = ESITO_ERROR;
007123150907         rpyOpCode = 'SQLCODE';
007124150907         rpyEsito = sqlCode;
007125150907         DUMP(A);
007126150907         RETURN ;
007127150907       ENDIF;
007128150907
007129150907       DOU sqlCode < 0;
007135150908
007136150907         EXEC SQL
007137150907           FETCH NEXT FROM CLIENTI
007138150907             INTO
007139171109                  :apd4pdr
007140171109                , :apd4cf
007141171109                , :apd4pi
007142171109                , :apd4ksc
007143171109                , :apd4soc
007144171201                , :apd4cel
007145171201                , :$rrn;
007146150907         IF sqlCode = 100;
007147150907           LEAVE;
007148150907         ELSEIF sqlCode < 0;
007149150907           rpyEsito = ESITO_ERROR;
007150150907           rpyOpCode = 'SQLCODE';
007151150907           rpyEsito = sqlCode;
007152150907           DUMP(A);
007153150907           LEAVE;
007154150907         ENDIF;
007155150907
007156150907         EXSR srctrwtr;
007157151023         if  *in99;
007158151023         EXEC SQL ROLLBACK;
007159151023           LEAVE;
007160151023         ENDIF;
007162150907
007163150907       ENDDO;
041900081015
042000151023       IF rpyOpCode = *BLANK and *in99='0';
042001151023         EXEC SQL commit;
042003171117         close fiapd01l;
042004171117         close fiapd41l;
042005171109         EXEC SQL DROP TABLE QTEMP/tl_CSV;
042006171109         EXEC SQL DROP TABLE QTEMP/tl_DB2;
042007171109         EXEC SQL DROP TABLE QTEMP/tl_ERR;
042008171201         ENDIF;
042500081015
042502150909         //__________________________________________________________
042503150909       begsr errori ;
042506171201           msg = %triml($msg) + ' ' + %triml($campo) +
042507151023           %triml(' numero record ') +
042508151022             %CHAR($rrn);
042509171221           if email_addAddress(mailto1) = 0;
042510171109             email_send('Cellulari autisti-Dati non validi'
042511151022             : msg);
042512150909           ENDIF;
042513171221           if email_addAddress(mailto2) = 0;
042514171221             email_send('Cellulari autisti-Dati non validi'
042515171221             : msg);
042516171221           ENDIF;
042517180207           if email_addAddress(mailto3) = 0;
042518180207             email_send('Cellulari autisti-Dati non validi'
042519180207             : msg);
042520180207           ENDIF;
042521150909         rpyEsito = ESITO_ERROR;
042522150910         *in99 = '1';
042523150909          ENDSR;
043600081015      /END-FREE
043900150907     C******************************************************
044000150907     C     srctrwtr      BEGSR
044001150907     C******************************************************
044002171117      * preparazione dati richiamo PGM variazioni
044003171117     c                   clear                   fnlv36ds
044004171201     c                   clear                   fiapdds
044006171117     c                   clear                   fiapd4b
044008171117     c                   eval      ILV36tip  = 'A'
044009171201     c                   eval      ILV36CTA  = 'M'
044010171221     c                   eval      ILV36PRU  = 'BART001'
044013171117     c                   eval      ILV36PGM  = 'FIDG66R'
044014171201     c* al momento non controllo correttezza contratto tariffa e e
044015171201     c* certificazione ma solo la correttezza "formale"
044016171201     c*
044098171117     c* controllo autista bloccante
046800171117     c                   exsr      sr_pdr
046900150907     c                   if        esito <> '0'
047100171117     c                   eval      $msg = 'Codice autista non trovato in -
047101171117     c                             anagrafica '
047102171109     c                   movel     apd4pdr       $campo
047200150907     c                   exsr      errori
047300150907     c                   leavesr
047400150907     c                   endif
048800171117      *cellulare bloccante
048801171213     c                   clear                   ok                1 0
048802171213     c                   if        apd4cel <> ' '
048803171213     c                   movel     apd4cel       num
048804171213     c*
048805171213     c                   do        16            x                 2 0
048810171213     c                   if        cel(x) <> ' ' and cel(x) <> '0'
048811171213     c                   eval      ok = 1
048812171213     c                   leave
048813171213     c                   end
048814171213     c                   enddo
048815171213     c*
048816171213     c                   if        ok = 1
049302171117     c                   exsr      sr_cel
049303171117     c                   if        esito <> '0'
049304171117     c                   eval      $msg = 'Nr. cellulare errato o mancante'
049305171117     c                   movel     apd4cel       $campo
049306171117     c                   exsr      errori
049307171117     c                   leavesr
049401171117     c                   endif
049402171213     c                   endif
049403171213     c                   endif
049411171117     c* codice fiscale
049412171117     c                   exsr      sr_cf
049413171117     c                   if        esito <> '0'
049414171201     c                   eval      $msg = 'Codice fiscale errato o mancante -
049415171201     c                             nelle certificazioni'
049416171117     c                   movel     apd4cf        $campo
049417171117     c                   exsr      errori
049418171201     c                   leavesr
049419171117     c                   endif
049420171117     c* codice fornitore
049421171117     c                   exsr      sr_ksc
049422171117     c                   if        esito <> '0'
049423171201     c                   eval      $msg = 'Codice fornitore errato o mancante -
049424171201     c                             nelle tariffe'
049425171117     c                   movel     apd4ksc       $campo
049426171117     c                   exsr      errori
049427171201     c                   leavesr
049428171117     c                   endif
049430171201     c* P.iva
049431171201     c                   exsr      sr_pi
049432171201     c                   if        esito <> '0'
049433171201     c                   eval      $msg = 'Partita iva errata o mancante -
049434171201     c                             nei contratti'
049435171201     c                   movel     apd4pi        $campo
049436171201     c                   exsr      errori
049437171201     c                   leavesr
049438171201     c                   endif
055207150907     c*
055209171117     c                   eval      apd4trd = 'TEL'
055210171213     c     kapd4         chain(e)  fiapd41l
055211171109     c                   if        not %error
055212171109     c                   if        %found(fiapd41l)
055213171117     c                   eval      fiapd4b  = fiapd4ds
055215171109     c                   movel     apd4uni       dapd4tel
055216171213     c                   if        ok = 1
055217171109     c                   movel     apd4cel       §apd4cel
055218171213     c                   else
055219171213     c                   clear                   §apd4cel
055220171213     c                   end
055221171201     c                   movel(p)  dapd4tel      apd4uni
055222171109     c                   update    fiapd400
055223171109     c                   else
055224171213     c* solo se ho un numero di cellulare
055225171213     c                   if        ok = 1
055226171109     c                   clear                   dapd4tel
055227171201     c                   clear                   fiapd4b
055228171109     c                   movel     apd4cel       §apd4cel
055229171201     c                   movel(p)  dapd4tel      apd4uni
055230171109     c                   write     fiapd400
055231171109     c                   end
055232171213     c                   end
055233171117     c                   end
055234171213     c* traccio storico se ho trovato il record o se ho il numero di cell.
055235171213     c                   if        %found(fiapd41l) or ok = 1
055236171221     c                   eval      ILV36NOJ  = 'S.NOW_TEL'
055237171117     c                   exsr      srstoria
055238171213     c                   end
055239171109     c* dati anagrafici fornitore che ha in comodato il telefono
055300171201     c                   eval      apd4trd = 'FOR'
055400171109     c*
055500171213     c     kapd4         chain(e)  fiapd41l
055600171109     c                   if        not %error
055700171109     c                   if        %found(fiapd41l)
055701171117     c                   eval      fiapd4b  = fiapd4ds
055800171201     c                   movel     apd4uni       dapd4for
055900171109     c                   movel     apd4ksc       §apd4ksc
055901171109     c                   movel     apd4soc       §apd4soc
055903171109     c                   movel     apd4pi        §apd4pi
056000171201     c                   movel(p)  dapd4for      apd4uni
056100171109     c                   update    fiapd400
056200171109     c                   else
056300171201     c                   clear                   dapd4for
056301171201     c                   clear                   fiapd4b
056303171109     c                   movel     apd4ksc       §apd4ksc
056304171109     c                   movel     apd4soc       §apd4soc
056305171109     c                   movel     apd4pi        §apd4pi
056306171201     c                   movel(P)  dapd4for      apd4uni
056600171109     c                   write     fiapd400
056700171109     c                   end
056701171117     c                   end
056702171117     c*
056703171221     c                   eval      ILV36NOJ  = 'S.NOW_FOR'
056704171117     c                   exsr      srstoria
058704171117     c*
058705171117     C                   ENDSR
058706171117     C******************************************************
058707171117     C     srSTORIA      BEGSR
058708171117     C******************************************************
058709171117     c                   call      'FNLV36R'
058710171117     c                   parm                    fnlv36ds
058711171117     c                   parm                    fiapdds
058712171201     c                   parm                    fiapdds
058713171117     c                   parm                    fiapd4b
058714171117     c                   parm                    fiapd4ds
058716171117     C                   ENDSR
058717171117     C******************************************************
058718171117     C     sr_pdr        BEGSR
058719171117     C******************************************************
058720171117     c                   eval      esito = '0'
058721171117     c     kapd          chain     fiapd01l
058722171117     c                   if        not %found(fiapd01l)
058723171117     c                   eval      esito = '1'
058724171117     c                   end
058725171117     C                   ENDSR
058800171117      /FREE
058900171117       //--------------------------------------------------------------
059000171117       //?Richiamo Service Program UBCHKCEL                             ?
059100171117       //--------------------------------------------------------------
059200171201       BEGSR sr_cel;
059300171117
059400171117         pInCell  = apd4cel;
059500171117         pOutCell = *blank;
059600171117         pOutErr = *blank;
059700171117
059800171117         if V1InCellOp = *blank;
059900171117           // è il NOPASS
060000171201           Esitocel = UBCHKCEL_Check ( pInCell : pOutCell :
060100171117                                      pOutErr );
060200171117         else;
060300171117           pInCellOpz = V1InCellOp;
060400171201           Esitocel = UBCHKCEL_Check ( pInCell : pOutCell :
060500171117                                      pOutErr: pInCellOpz );
060600171117         endif;
060700171117
060800171201         apd4cel = pOutCell;
060801171201         if pouterr = 'E';
060802171201         esito   = '1';
060803171201         endif;
061000171117
061100171117       ENDSR;
061200171117      /END-FREE
061300171117     C******************************************************
061400171117     C     sr_cf         BEGSR
061500171117     C******************************************************
061600171117     c                   eval      esito = '0'
061601171201     c                   clear                   conta             3 0
061700171201     C/EXEC SQL
061800171201     C+ SET :conta = (SELECT COUNT(*) FROM AITRA00F WHERE
061801171201     C+ trakau = :apd4pdr and tracf = :apd4cf)
061900171201     C/END-EXEC
061901171201     c                   if        conta = 0
061902171201     c                   eval      esito = '1'
061903171201     c                   end
062100171117     C                   ENDSR
062200171201     C******************************************************
062300171201     C     sr_ksc        BEGSR
062400171201     C******************************************************
062500171201     c                   eval      esito = '0'
062600171201     c                   clear                   conta             3 0
062601171201     c                   eval      cdf = '00000000'
062602171220     c                   move      apd4ksc       cdf               8
062700171201     C/EXEC SQL
062800171201     C+ SET :conta = (SELECT COUNT(*) FROM fitgt00F WHERE
062900171201     C+ tgtpdr = :apd4pdr and tgtsoc= :apd4soc and tgtcdf=:cdf)
063000171201     C/END-EXEC
063100171201     c                   if        conta = 0
063200171201     c                   eval      esito = '1'
063300171201     c                   end
063400171201     C                   ENDSR
063500171201     C******************************************************
063600171201     C     sr_pi         BEGSR
063700171201     C******************************************************
063800171201     c                   eval      esito = '0'
063900171201     c                   clear                   conta             3 0
064200171201     C/EXEC SQL
064300171201     C+ SET :conta = (SELECT COUNT(*) FROM aitrs00F WHERE
064400171201     C+ trsiva = :apd4pi)
064500171201     C/END-EXEC
064600171201     c                   if        conta = 0
064700171201     c                   eval      esito = '1'
064800171201     c                   end
064900171201     C                   ENDSR
