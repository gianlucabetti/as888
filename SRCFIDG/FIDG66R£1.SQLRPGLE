000100081008     ***************************************************************************
000200081008     **
000400150907     ** scrive tnstp00f se i dati sono corretti
000500081223     **
001100081008     **
001200081008     ***************************************************************************
001201150908     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('QC2LE')
001202150908     H BNDDIR('PJXBND')
001203150909     H BNDDIR('TRUL')
001400081030
001500081030     ***************************************************************************
001600081030     **
001700081030     ** Definizioni costanti.
001800081030     **
001900081030     ***************************************************************************
001901151023     FTnstp01l  iF a E           K DISK    usropn commit
001902150907     FTntbe01l  IF   E           K DISK
001903150907     Fanuni01l  IF   E           K DISK
001906151022     D kpjba         E DS                  inz
001907151223     D ana330ds      E DS
001909150918     D ycostp1ds     e ds                  prefix(£)
001913150909     D psds           SDS
001914150909     D  jobName              244    253A
001915150909     D  jobUser              254    263A
001916150909     D  jobNumber            264    269A
001917150907     D nd002ds       e ds                  inz
001918150907     D xsoc001ds     e ds                  inz
001919150907     D dtce          E DS
001920151023     D srce            s              3    DIM(60)
001921151023     d i               s              3  0 inz
001922151023     d x               s              3  0 inz
001923150907     D DTAcor          S               D   INZ
001924150907     D DaTAiso         S               D   INZ
001925151022     D alfa8           S              8    inz
001926150907     D DaTAEUR         S               D   DATFMT(*EUR) INZ
001927150909     D REPLACE...
001928150909     D                 C                   'R'
001929151022     D msg             S            300
001930151022     D $msg            S            100
001931150909     D $campo          S             30
001932150909     D $rrn            S              9  0
001933150907     D Esito           S              1
001934150907     D GesMsg          S              1
001935150907     D Output          S           4000
001936150907     D LenOut          S              9B 0
001937150907     D StrutturaO      S             10
001938150907     D Refresh         S              1
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
004601151106     D tivlr00f      E DS                  extname(tivlr00f)
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
005801151106     C                   PARM                    tivlr00f
005803150907     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
005804150907     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
005805150907     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
005806150907     C                   move      WN8           dataeur
005807150907     C                   move      dataeur       DtaCOR
005808150907     C*
005809151022     C                   EVAL      SOCXSC = '201'
005810151022     C                   CALL      'XSOC'
005811151022     C                   PARM      'SOC001'      TIPXSC            6
005812151022     C                   PARM                    SOCXSC            3
005813151022     C                   PARM                    CDSXSC            9 0
005814151022     C                   PARM                    MODXSC            3
005815151022     C                   PARM                    RTNXSC            1
005816151022     C                   PARM                    XSOC001DS
005817151022     C                   PARM                    KPJBA
005824150907      *
005825150907     C     Kuni          KLIST                                                  *
005826150907     C                   KFLD                    xscsoc                         *
005827150907     C                   KFLD                    ycostpcdcd                     *
005835150907      *
005836150907     C* POSIZIONAMENTO TABELLE                             *
005837150907     C     KTbe          KLIST                                                  *
005838150907     C                   KFLD                    TBECOD                         *
005839150907     C                   KFLD                    TBEKE1                         *
005840150907     C                   KFLD                    TBEKE2                         *
005841151023     C     KTbe1         KLIST                                                  *
005842151023     C                   KFLD                    TBECOD                         *
005843151023     C                   KFLD                    TBEKE1                         *
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
007102151023       open tnstp01l;
007103151023
007104150907         // leggo l'archivio in QTEMP
007105150907       EXEC SQL
007106150907         DECLARE CLIENTI NO SCROLL CURSOR FOR
007107150915           SELECT
007108150909                  causale,
007109150908                  importo,
007110150908                  competenza,
007111150915             case when capoconto is null then '000000' else capoconto end,
007112150915             case when sottoconto is null then '00000000' else sottoconto end,
007113150915             case when cdc is null then '00000000' else cdc end,
007114150915             case when molti is null then 0 else molti end,
007115150915             rrn(addstp_db2)
007116150907           FROM addstp_db2;
007117150907       EXEC SQL
007118150907         OPEN CLIENTI;
007119150907
007120150907       IF sqlCode < 0;
007121150907         rpyEsito = ESITO_ERROR;
007122150907         rpyOpCode = 'SQLCODE';
007123150907         rpyEsito = sqlCode;
007124150907         DUMP(A);
007125150907         RETURN ;
007126150907       ENDIF;
007127150907
007128150907       DOU sqlCode < 0;
007129150907
007130150908       reset tnstp000;
007131150908
007132150907         EXEC SQL
007133150907           FETCH NEXT FROM CLIENTI
007134150907             INTO
007135150915                  :ycostptre
007136150907                , :ycostpimp
007137151022                , :alfa8
007138150907                , :YCOSTPkccd
007139150907                , :YCOSTPkscd
007140150907                , :YCOSTPcdcd
007141150915                , :YCOSTPmol
007142150915                , :$rrn;
007143150907         IF sqlCode = 100;
007145150907           LEAVE;
007147150907         ELSEIF sqlCode < 0;
007148150907           rpyEsito = ESITO_ERROR;
007149150907           rpyOpCode = 'SQLCODE';
007150150907           rpyEsito = sqlCode;
007151150907           DUMP(A);
007152150907           LEAVE;
007153150907         ENDIF;
007154150907
007155150907         EXSR srctrwtr;
007156151023         if  *in99;
007158151023         EXEC SQL ROLLBACK;
007159151023           LEAVE;
007160151023         ENDIF;
007162150907
007163150907       ENDDO;
041900081015
042000151023       IF rpyOpCode = *BLANK and *in99='0';
042001151023         EXEC SQL commit;
042002151023         close tnstp01l;
042003151023         EXEC SQL DROP TABLE QTEMP/ADDSTP_CSV;
042004151023         EXEC SQL DROP TABLE QTEMP/ADDSTP_DB2;
042005151023         EXEC SQL DROP TABLE QTEMP/ADDSTP_ERR;
042006151023         // contabilizzo
042007151023         exsr contab;
042008151023            if £rpyEsito = 0;
042100151023            rpyOpCode = 'DONE';
042200151023            ELSEIF rpyOpCode = 'SQLCODE';
042300151221            ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
042301151023            ENDIF;
042400081015       ENDIF;
042500081015
042502150909         //__________________________________________________________
042503150909       begsr errori ;
042504151023           msg = 'Causale ' + %triml(ycostptre) + ' '
042505151023           + %triml($msg) + ' ' + %triml($campo) +
042506151023           %triml(' numero record ') +
042507151022             %CHAR($rrn);
042508151022           if email_addAddress(mailto) = 0;
042509151022             email_send('Note di addebito in CE-Dati non validi'
042510151022             : msg);
042511150909           ENDIF;
042512150909         rpyEsito = ESITO_ERROR;
042513150910         *in99 = '1';
042514150909          ENDSR;
043600081015      /END-FREE
043900150907     C******************************************************
044000150907     C     srctrwtr      BEGSR
044001150907     C******************************************************
044004150907     c*
044005151022     c                   if        alfa8 <> ' '
044006151022     c                   move      alfa8         data8             8 0
044007151022     c                   move      data8         dataiso
044008151022     c                   move      dataiso       YCOSTPini
044009151022     c                   else
044010151022     c                   move      dtacor        YCOSTPini
044011151022     c                   end
044017151023     c* normalizzo il cdc
044018151023     c                   if        ycostpcdcd <> ' '  and
044019151023     c                             %subst(ycostpcdcd: 3: 1) = ' '
044020151023     c                   movel     ycostpcdcd    com3              3
044021151023     c                   select
044022151023     c                   when      %subst(ycostpcdcd: 2: 1) = ' '
044023151023     c                   eval      ycostpcdcd = '00' +
044025151023     c                             %subst(com3: 1: 1)
044026151023     c                   other
044027151023     c                   eval      ycostpcdcd = '0' +
044028151023     c                             %subst(com3: 1: 2)
044029151023     c                   endsl
044030151023     c                   end
044031151023     c* normalizzo il kcc
044032151023     c                   if        ycostpkccd <> ' '  and
044033151023     c                             %subst(ycostpkccd: 6: 1) = ' '
044034151023     c                   movel     ycostpkccd    com6              6
044035151023     c                   select
044036151023     c                   when      %subst(ycostpkccd: 2: 1) = ' '
044037151023     c                   eval      ycostpkccd = '00000' +
044038151023     c                             %subst(com6: 1: 1)
044039151023     c                   when      %subst(ycostpkccd: 3: 1) = ' '
044040151023     c                   eval      ycostpkccd = '0000' +
044041151023     c                             %subst(com6: 1: 2)
044042151023     c                   when      %subst(ycostpkccd: 4: 1) = ' '
044043151023     c                   eval      ycostpkccd = '000' +
044044151023     c                             %subst(com6: 1: 3)
044045151023     c                   when      %subst(ycostpkccd: 5: 1) = ' '
044046151023     c                   eval      ycostpkccd = '00' +
044047151023     c                             %subst(com6: 1: 4)
044048151023     c                   other
044049151023     c                   eval      ycostpkccd = '0' +
044050151023     c                             %subst(com6: 1: 5)
044051151023     c                   endsl
044052151023     c                   end
044053151023     c* normalizzo il ksc
044054151023     c                   if        ycostpkscd <> ' '  and
044055151023     c                             %subst(ycostpkscd: 8: 1) = ' '
044056151023     c                   movel     ycostpkscd    com8              8
044057151023     c                   select
044058151023     c                   when      %subst(ycostpkscd: 2: 1) = ' '
044059151023     c                   eval      ycostpkscd = '0000000' +
044060151023     c                             %subst(com8: 1: 1)
044061151023     c                   when      %subst(ycostpkscd: 3: 1) = ' '
044062151023     c                   eval      ycostpkscd = '000000' +
044063151023     c                             %subst(com8: 1: 2)
044064151023     c                   when      %subst(ycostpkscd: 4: 1) = ' '
044065151023     c                   eval      ycostpkscd = '00000' +
044066151023     c                             %subst(com8: 1: 3)
044067151023     c                   when      %subst(ycostpkscd: 5: 1) = ' '
044068151023     c                   eval      ycostpkscd = '0000' +
044069151023     c                             %subst(com8: 1: 4)
044070151023     c                   when      %subst(ycostpkscd: 6: 1) = ' '
044071151023     c                   eval      ycostpkscd = '000' +
044072151023     c                             %subst(com8: 1: 5)
044073151023     c                   when      %subst(ycostpkscd: 7: 1) = ' '
044074151023     c                   eval      ycostpkscd = '00' +
044075151023     c                             %subst(com8: 1: 6)
044076151023     c                   other
044077151023     c                   eval      ycostpkscd = '0' +
044078151023     c                             %subst(com8: 1: 7)
044079151023     c                   endsl
044080151023     c                   end
044081151023     c* controllo che se scelta causale SDP la data deve essere
044082151023     c* dell'anno precedente (CHIODO)
044083160511     c*                  if        ycostptre = 'SDP'
044084160511     c                   if        1 <> 1
044085151023     c                   extrct    ycostpini:*y  annoini           4 0
044086151023     c                   if        *year = annoini
044087151023     c                   eval      $msg = 'Data errarata per causale SDP'
044088151023     c                   movel     ycostpini     $campo
044089151023     c                   exsr      errori
044090151023     c                   leavesr
044091151023     c                   endif
044092151023     c                   endif
044093151023     c* controllo dati in tabella
044094151023     c                   exsr      srtbe
044095151023     c                   if        *in99
044096151023     c                   leavesr
044097151023     c                   end
044098151023     c* controllo capoconto e sottoconto dare
046800150907     c                   exsr      contrCod
046900150907     c                   if        esito <> '0'
047100150907     c                   eval      $msg = 'Codice non trovato in anagrafico'
047101150909     c                   movel     ycostpkscd    $campo
047200150907     c                   exsr      errori
047300150907     c                   leavesr
047400150907     c                   endif
048800150907      *importo
048900151221     c                   if        ycostpimp = 0 or
048901151221     c                             ycostpimp < 0
049100151221     c                   eval      $msg = 'Importo errato o mancante'
049101150909     c                   movel     ycostpimp     $campo
049200150907     c                   exsr      errori
049300150907     c                   leavesr
049400150907     c                   endif
049500150907     c                   if        ycostpimp > 0 and ycostpimp > §tceimp
049700150907     c                   eval      $msg = 'Importo superiore al limite ammesso'
049701150909     c                   movel     ycostpimp     $campo
049800150907     c                   exsr      errori
049900150907     c                   leavesr
050000150907     c                   endif
050100150910      *tipo moltiplicatore opzionale
050200150907     c                   if        ycostpmol > 0 and ycostpmol > §tcemol
050400150907     c                   eval      $msg = 'Nr. registrazioni -
050500150907     c                             maggiore del limite ammesso'
050501150909     c                   movel     ycostpmol     $campo
050600150907     c                   exsr      errori
050700150907     c                   leavesr
050800150907     c                   endif
050802150907     c                   if        ycostpmol = 0
050804150907     c                   eval      ycostpmol = §tcemol
050805150907     c                   end
050900150907      *data iniziale
051000150907     c                   if        ycostpini = *loval
051200150907     c                   eval      $msg = 'Data inizio obbligatoria '
051201150909     c                   movel     ycostpini     $campo
051300150907     c                   exsr      errori
051400150907     c                   leavesr
051500150907     c                   end
052800150908     c                   if        ycostpini < dtacor
053100150907     c*                  eval      $msg = 'Data inizio deve essere -
053200150907     c*                            maggiore uguale a oggi'
053201150909     c*                  movel     ycostpini     $campo
053400150907     c*                  exsr      errori
053500150909     c*                  leavesr
053600150907     c                   endif
053601150910     c* centro di costo dare in questo momento deve esserci
054300150907     c     kuni          chain     anuni01l
054400150907     c                   if        not %found(anuni01l)
054800151023     c                   eval      $msg = 'CDC dare inesistente'
054801150909     c                   movel     ycostpcdcd    $campo
054900150907     c                   exsr      errori
055000150907     c                   leavesr
055200150907     c                   endif
055201150909     C*
055202150909     c                   move      *loval        ycostpela
055203150909     c                   move      jobuser       ycostpute
055204150909     c                   move      dtacor        ycostpupd
055205151106     c                   move      oracor        ycostporaa
055206151112     c                   eval      ycostphdl = %subst(vlrmsg: 51: 10)
055207150907     c*
055208150907     c                   write     tnstp000
055209151023     c                   z-add     1             i
055210151023     c     ycostptre     lookup    srce(i)                                01
055211151023     c                   if        not *in01
055212151023     c                   add       1             x
055213151023     c                   eval      srce(x) = ycostptre
055214151023     c                   end
055215151023     C                   ENDSR
055216151023     C******************************************************
055217151023     c     contab        begsr
055218151023     C******************************************************
055219151023     c                   do        60            i
055220151023     c                   if        srce(i) <> ' '
055221150918     c                   clear                   ycostp1ds
055222150918     c                   eval      £societa = '201'
055223151023     c                   eval      £tipo = srce(i)
055224151112     c                   eval      £hdl = %subst(vlrmsg: 51: 10)
055225160129     c*                  call      'YCOSTP1R'
055226160129     c*                  parm                    ycostp1ds
055227151023     c                   end
055228151023     c                   enddo
055229150907     c*
055700150907     C                   ENDSR
060401150907     C******************************************************
060500150907     c     contrCod      begsr
060601150907     C******************************************************
060602151223     c                   clear                   ana330ds
060603151223     c                   do        *hival
060700150907     C                   EVAL      LenOut = %Size(ND002DS)
060800150907     C                   callb     'NDMVC002'
060900150907     C                   PARM                    xscsoc
061000150907     C                   PARM                    ycostpkccd
061100150907     C                   PARM                    ycostpkscd
061200150907     C                   PARM                    dtacor
061300150907     C                   PARM      '0'           GesMsg
061400150907     C                   PARM      *blanks       Esito
061500150907     C                   PARM      'ND002DS'     StrutturaO
061600150907     C     nd002ds       PARM                    output
061700150907     C                   PARM                    LenOut
061800160114     C                   PARM      '1'           Refresh
061801151223     c                   if        esito = '0'
061802151223     c                   leavesr
061803151223     c                   end
061804151223     c*provo ad inserire il codice 8888 solo la 1° volta
061805151223     c                   if        err330 = ' '
061806151223     C                   MOVE      ycostpkscd    ksc330
061807151223     c                   movel     ana330ds      kpjbu
061808151223     C                   CALL      'YCO102R'
061809151223     C                   PARM                    kpjba
061810151223     C                   movel     kpjbu         ANA330DS
061811151223     c                   if        err330 = '0'
061812151223     c                   iter
061813151223     c                   end
061814151223     c                   end
061815151223     c                   leave
061818151223     c                   enddo
061819150907     c                   endsr
061820150907      *
061821150907     C******************************************************
061822150907     c     srtbe         begsr
061823150907     C******************************************************
061824150910     c                   setoff                                       99
061825150907     c                   movel(p)  ycostptre     tbeke1
061826150907     c                   move      'TCE'         tbecod
061827151023     c     ktbe1         chain     tntbe01l
061828150907     c                   if        %found(tntbe01l)
061829150907     C                   MOVEL     TBeUNI        dtce
061830150909     c                   else
061831150909     c                   eval      $msg = 'Causale TCE non trovata'
061832150909     c                   movel     ycostptre     $campo
061833150909     c                   exsr      errori
061834150909     c                   leavesr
061835150907     c                   endif
061836151023     c* mail per invio errori
061837150907     c                   move      'RCE'         tbecod
061838150915     c                   movel(p)  'MAILTO'      tbeke2
061839150915     c     ktbe          chain     tntbe01l
061840150915     c                   if        %found(tntbe01l)
061841150915     C                   MOVEL     TBeUNI        mailto           50
061842150915     c                   else
061843151023     c                   eval      $msg = 'Mail RCE non trovata'
061844150915     c                   movel     tbeke2        $campo
061845150915     c                   exsr      errori
061846150915     c                   leavesr
061847150915     c                   endif
061848151023     c* mail per invio errori
061849151023     c                   move      'RCE'         tbecod
061850151023     c                   movel(p)  'CAUSALE'     tbeke2
061851151023     c     ktbe          chain     tntbe01l
061852151023     c                   if        %found(tntbe01l)
061853151023     C                   MOVEL     TBeUNI        ycostpcau
061854151023     c                   else
061855151023     c                   eval      $msg = 'Causale contabil RCE non trovata'
061856151023     c                   movel     tbeke2        $campo
061857151023     c                   exsr      errori
061858151023     c                   leavesr
061859151023     c                   endif
061860150918      * voce avere non è obbligatoria (es. addebiti danni x anno precedente)
061861150907     c                   movel(p)  'VOCEAVERE'   tbeke2
061862150907     c     ktbe          chain     tntbe01l
061863150907     c                   if        %found(tntbe01l)
061864150907     C                   MOVEL     TBeUNI        ycostpvoca
061865150929     c                   else
061866150929     c                   eval      $msg = 'Voce AVERE RCE non trovata'
061867150929     c                   movel     tbeke2        $campo
061868150929     c                   exsr      errori
061869150929     c                   leavesr
061870150910     c                   endif
061871150915     c* cdc avere se non c'è lo metto = al cdc dare
061872150915     c                   movel(p)  'CDCAVERE'    tbeke2
061873150915     c     ktbe          chain     tntbe01l
061874150915     c                   if        %found(tntbe01l)
061875150915     C                   MOVEL     TBeUNI        ycostpcdca
061876150929     c                   else
061877150929     c                   eval      $msg = 'CdC AVERE RCE non trovata'
061878150929     c                   movel     tbeke2        $campo
061879150929     c                   exsr      errori
061880150929     c                   leavesr
061881150915     c                   endif
061882150907      *
061883150907     c                   movel(p)  'CONTOAVERE'  tbeke2
061884150907     c     ktbe          chain     tntbe01l
061885150907     c                   if        %found(tntbe01l)
061886150907     C                   eval      ycostpkcca = %subst(tbeuni:1:6)
061887150907     C                   eval      ycostpksca = %subst(tbeuni:7:8)
061888150909     c                   else
061889151023     c                   eval      $msg = 'Conto avere RCE non trovata'
061890150909     c                   movel     tbeke2        $campo
061891150909     c                   exsr      errori
061892150909     c                   leavesr
061893150907     c                   endif
061894150910      * voce dare
061895150910     c                   movel(p)  'VOCEDARE'    tbeke2
061896150910     c     ktbe          chain     tntbe01l
061897150910     c                   if        %found(tntbe01l)
061898150910     C                   MOVEL     TBeUNI        ycostpvocd
061899150910     c                   else
061900151023     c                   eval      $msg = 'Voce dare RCE non trovata'
061901150910     c                   movel     tbeke2        $campo
061902150910     c                   exsr      errori
061903150910     c                   leavesr
061904150910     c                   endif
061905150910      * sottoconto dare
061906150918     c                   if        ycostpkscd = ' ' or
061907151022     c                             ycostpkscd = '00000000'
061908150910     c                   movel(p)  'CONTODARE'   tbeke2
061909150910     c     ktbe          chain     tntbe01l
061910150910     c                   if        %found(tntbe01l)
061911150910     C                   eval      ycostpkccd = %subst(tbeuni:1:6)
061912150910     C                   eval      ycostpkscd = %subst(tbeuni:7:8)
061913150910     c                   else
061914151023     c                   eval      $msg = 'Conto dare RCE non trovata'
061915150910     c                   movel     tbeke2        $campo
061916150910     c                   exsr      errori
061917150910     c                   leavesr
061918150910     c                   endif
061919150910     c                   else
061920150910     c* capoconto dare
061921150910     c                   if        ycostpkccd = ' ' or
061922150910     c                             ycostpkccd = '000000'
061923150910     c                   movel(p)  'CAPODARE'    tbeke2
061924150910     c     ktbe          chain     tntbe01l
061925150910     c                   if        %found(tntbe01l)
061926151023     C                   MOVEL     TBeUNI        ycostpkccd
061927150910     c                   else
061928151023     c                   eval      $msg = 'Capoconto dare RCE non trovata'
061929150910     c                   movel     tbeke2        $campo
061930150910     c                   exsr      errori
061931150910     c                   leavesr
061932150910     c                   endif
061933150910     c                   end
061934150910     c                   endif
061935150910      * cdc dare
061936150910     c                   if        ycostpcdcd = ' ' or
061937150910     c                             ycostpcdcd = '00000000'
061938150910     c* se è un cliente prendo i primi tre byte del codice
061939150910     c                   if        ycostpkccd = '008888'
061940150910     c                   eval      ycostpcdcd = %subst(ycostpkscd: 2: 3)
061941150910     c                   else
061942150910     c* verifico se presente in tabella
061943150910     c                   movel(p)  'CDCDARE'     tbeke2
061944150910     c     ktbe          chain     tntbe01l
061945150910     c                   if        %found(tntbe01l)
061946150910     C                   MOVEL     TBeUNI        ycostpcdcd
061947150929     c                   else
061948150929     c                   eval      $msg = 'CdC DARE RCE non trovata'
061949150929     c                   movel     tbeke2        $campo
061950150929     c                   exsr      errori
061951150929     c                   leavesr
061952150910     c                   endif
061953150915     c                   endif
061954150910     c* se ancora vuoto lo imposto uguale a quello avere
061955150918     c                   if        ycostpcdcd = ' '
061956150918     c                   if        ycostpcdca <>' '
061957150915     c                   eval      ycostpcdcd = ycostpcdca
061958150915     c                   else
061959150915     c                   eval      $msg = 'CDC dare non trovato'
061960150910     c                   clear                   $campo
061961150910     c                   exsr      errori
061962150910     c                   leavesr
061963150910     c                   endif
061964150918     c                   endif
061965150910     c                   endif
061966150915     c* se cdc avere non trovato lo imposto uguale al dare
061967150915     c                   if        ycostpcdcd <> ' ' and
061968151022     c                             ycostpcdca = ' '  and
061969151022     c                             ycostpvoca <>' '
061970150915     c                   eval      ycostpcdca = ycostpcdcd
061971151022     c                   end
061976150907     c*
062000150907     C                   endsr
