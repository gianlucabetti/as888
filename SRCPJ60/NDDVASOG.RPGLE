000100970324     H*BIND
000200980722PIG0 H*CRTPGM BNDDIR(PJXBND) ACTGRP(QILE) OPTION(*DUPPROC)
000300941216     H DECEDIT('0,') DATEDIT(*DMY.)
000400941216      *----------*
000500941216      *
000600950925      *       DRIVER ACCESSO AI SOGGETTI CLIENTI e FORNITORI
000700941216      *
000800950925      *----------*
000900950925      * Parametri "sciolti": INPUT
001000941216      *
001100950607      * DVARIC     Operazione richiesta
001200950607      *            1=CHAIN
001300950607      *            2=SETLL e READE
001400950607      *            3=READE
001500950607      *            9=SETLL
001600950607      * DVASOCIETA Societa
001700950607      * DVAUNITA   unità
001800950615      * DVASTRUTT  informazioni da ottenere
001900950925      *            DVACLI          (accesso 1)
002000950925      *            DVAFOR
002100950925      *            ANCLN
002200950925      *            ANFRN
002300950925      *            DVASOG          (accesso 2)
002400950925      *            ANIND
002500950925      *            ANSOG
002600100723C2163 *            ANSO1
002700950607      * DVADTRIF   data di riferimento
002800950607      * DVALENOUT  lunghezza informazioni
002900950922      * ---------------------------
003000950922      *    Accessi in alternativa
003100950922      * ---------------------------
003200950922      *    Accesso 1: per partenza da ANCLN o ANFRN/ANAPE
003300950922      *               per andare anche su ANSOG e ANIND
003400950607      * DVASNATURA
003500950607      * DVACODICE
003600950607      * DVALINEAV
003700950607      * DVAFILIALE
003800950922      *
003900950922      *    Accesso 2: per lettura ANSOG e/o ANIND senza accesso a CLN/FRN
004000950607      * DVASOGG
004100950922      * --------------
004200950922      *    Specifica dell'indirizzo da reperire
004300950922      *    per tutti e due gli accessi
004400950925      * DVATPIND
004500950925      * DVACDIND
004600950925      *    se non immessi (caso di *omit)
004700950925      *    vengono reperiti il tp indirizzo e cd indirizzo
004800950925      *    presente su ANCLN o su ANFRN se si tratta di accesso 1, nel caso
004900950925      *    di accesso 2 sono impostati a blank
005000950925      *
005100950922      * --------------
005200950607      * OUTPUT
005300950925      * --------------
005400950607      *
005500950615      * DVAOUTPUT  informazioni ottenute
005600950607      * DVAERRORE
005700950607      *            Errore
005800950607      *            0=nessun errore
005900950607      *            1=non trovato
006000950607      *            2=record allocato
006100941216      *
006200941216     F*----------------------------------------------------*
006300980722     FANSOG01L  IF   E           K DISK    usropn
006400980722     FANIND01L  IF   E           K DISK    usropn
006500980722     FANCLN01L  IF   E           K DISK    usropn
006600980722     FANFRN01L  IF   E           K DISK    usropn
006700980722     FANAPE01L  IF   E           K DISK    usropn
006800100723C2163FANSO101L  IF   E           K DISK    usropn
006900941216     D*----------------------------------------------------*
007000961008     D KPJBA         E DS
007100961008     D*-------------
007200961008     D* Dati per pgm XSOC
007300961008     D SOC001        E DS                  EXTNAME(XSOC001DS)
007400961008     D    FgO5                        1    overlay(XscFgo:5)                    Scadenz. retroattivo
007500961008     D*-------------
007600961008     D* DS Interna per dati utente
007700961008     D XSOCDS          DS          1000
007800961008     D*-------------
007900941216     D* Reperimento nome PGM
008000941216     D STATUS         SDS           333
008100941216     D  DSPGM            *PROC
008200941216     D*
008300950607     D*  Ds per output DVASOG
008400950607     D DVASOG        E DS                  INZ
008500950607     D*  Ds per output DVACLI
008600950607     D DVACLI        E DS                  INZ
008700950607     D*  Ds per output DVAFOR
008800950607     D DVAFOR        E DS                  INZ
008900950925     D*  Ds per output
009000950804     D ANIND         E DS                  EXTNAME(ANIND00F) inz
009100950925     D*  Ds per output
009200950925     D ANCLN         E DS                  EXTNAME(ANCLN00F) inz
009300950925     D*  Ds per output
009400950925     D ANFRN         E DS                  EXTNAME(ANFRN00F) inz
009500950925     D*  Ds per output
009600950925     D ANSOG         E DS                  EXTNAME(ANSOG00F) inz
009700100723C2163D*  Ds per output
009800100723C2163D ANSO1         E DS                  EXTNAME(ANSO100F) inz
009900950607     D*Campi di lavoro
010000950607     D WRKCODICE       S                   like(DVACODICE)
010100950607     D WRKFILIALE      S                   like(DVAFILIALE)
010200950607     D WRKLINEAV       S                   like(DVAlineav)
010300950607     D WRKsnatura      S                   like(DVAsnatura)
010400950607     D WRKSOGG         S                   like(DVAsogg)
010500950612     D*-------------
010600950612     D* Definizioni parametri driver soggetti
010700950612     DDvaric           S              1
010800950612     DDvaSocieta       S              3
010900950612     DDvaUnita         S              8
011000950615     DDvaStrutt        S             10
011100950612     DDvaDtRif         S               D
011200950612     DDvaLenOut        S              9B 0
011300950612     DDvaSnatura       S              1
011400950612     DDvaCodice        S              8
011500950612     DDvaLineav        S              3
011600950612     DDvaFiliale       S              3  0
011700950612     DDvaSogg          S              8
011800950612     DDvaTpInd         S              2
011900950612     DDvaCdInd         S              3
012000950612     DDvaErrore        S              1
012100950615     DDvaoutput        S           4000
012200980722     D*-------------
012300980722     D OpenSOG         S              1    inz(*off)
012400980722     D OpenIND         S              1    inz(*off)
012500980722     D OpenCLN         S              1    inz(*off)
012600980722     D OpenFRN         S              1    inz(*off)
012700980722     D OpenAPE         S              1    inz(*off)
012800100723C2163D OpenSO1         S              1    inz(*off)
012900980907     D*-------------
013000980907     Dcinque           s              5
013100941216     C*----------------------------------------------------*
013200941216     C*                MAIN LINE PROGRAM
013300941216     C*----------------------------------------------------*
013400941216     C*
013500941216     C* inizializzazione variabili
013600941216     C                   EXSR      INZVAR
013700941216     C*
013800941216     C* Informazioni richieste
013900941216     C                   SELECT
014000950607     C* cli
014100950616     C     $OPE          WHENEQ    'CLI'
014200950607     C                   EXSR      DATCLI
014300950607     C* for
014400950616     C     $OPE          WHENEQ    'FOR'
014500950607     C                   EXSR      DATFOR
014600950607     C* sog
014700950616     C     $OPE          WHENEQ    'SOG'
014800950607     C                   EXSR      DATSOG
014900941216     C*
015000941216     C                   ENDSL
015100950616     c*
015200950616     C     DVAERRORE     IFEQ      *OFF
015300950616     C*
015400950616     C* Riempie dati di ritorno
015500950616     C                   SELECT
015600950616     C     DVASTRUTT     WHENEQ    'DVACLI'
015700950616     C                   EXSR      RIECLI
015800950616     C                   eval      %subst(DVAOUTPUT:1:DVALENOUT)
015900950616     C                              = %subst(DVACLI:1:DVALENOUT)
016000950925     C*
016100950804     C     DVASTRUTT     WHENEQ    'ANIND'
016200950804     C                   eval      %subst(DVAOUTPUT:1:DVALENOUT)
016300950804     C                              = %subst(ANIND:1:DVALENOUT)
016400950925     C*
016500950925     C     DVASTRUTT     WHENEQ    'ANSOG'
016600950925     C                   eval      %subst(DVAOUTPUT:1:DVALENOUT)
016700950925     C                              = %subst(ANSOG:1:DVALENOUT)
016800950925     C*
016900950925     C     DVASTRUTT     WHENEQ    'ANCLN'
017000950925     C                   eval      %subst(DVAOUTPUT:1:DVALENOUT)
017100950925     C                              = %subst(ANCLN:1:DVALENOUT)
017200950925     C*
017300950925     C     DVASTRUTT     WHENEQ    'ANFRN'
017400950925     C                   eval      %subst(DVAOUTPUT:1:DVALENOUT)
017500950925     C                              = %subst(ANFRN:1:DVALENOUT)
017600950616     C*
017700950616     C     DVASTRUTT     WHENEQ    'DVAFOR'
017800950616     C                   EXSR      RIEFOR
017900950616     C                   eval      %subst(DVAOUTPUT:1:DVALENOUT)
018000950616     C                              = %subst(DVAFOR:1:DVALENOUT)
018100950616     C*
018200950616     C     DVASTRUTT     WHENEQ    'DVASOG'
018300950616     C                   EXSR      RIESOG
018400950616     C                   eval      %subst(DVAOUTPUT:1:DVALENOUT)
018500950616     C                              = %subst(DVASOG:1:DVALENOUT)
018600100723C2163C*
018700100723  "  C     DVASTRUTT     WHENEQ    'ANSO1'
018800100723  "  C                   eval      %subst(DVAOUTPUT:1:DVALENOUT)
018900100723  "  C                              = %subst(ANSO1:1:DVALENOUT)
019000100723C2163C*
019100950616     C                   ENDSL
019200950616     C                   ENDIF
019300950607     C*
019400950607     C                   EXSR      ENDPGM
019500941216     C*
019600950607     C/EJECT
019700950607     C************************************************************
019800950607     C* FINE PROGRAMMA
019900950607     C************************************************************
020000950607     C     ENDPGM        BEGSR
020100950607     C*
020200941216     C                   SETON                                            RT
020300950607     C                   RETURN
020400950607     C*
020500950607     C                   ENDSR
020600941216     C/EJECT
020700941216     C************************************************************
020800950607     C* ROUTINE PER REPERIRE DATI PER DS DVACLI
020900941216     C************************************************************
021000950607     C     DATCLI        BEGSR
021100941216     C*
021200950607     C                   MOVE      DVASOCIETA    CLNSOCIETA
021300950607     C                   MOVE      WRKCODICE     CLNKSC
021400950607     C                   MOVE      WRKFILIALE    CLNFILIALE
021500950607     C                   MOVE      WRKLINEAV     CLNLINEAV
021600950607     C     K4CLN01       CHAIN     ANCLN01L                           2122
021700941216     C*
021800941216     C* se il rcd da gestire non viene trovato
021900941216     C* oppure è allocato da altro pgm
022000941216     C* ritorno al pgm chiamante avvertendolo
022100941216   2SC                   SELECT
022200941216     C     *IN21         WHENEQ    *ON
022300950607     C                   MOVE      '1'           DVAERRORE
022400941216     C     *IN22         WHENEQ    *ON
022500950607     C                   MOVE      '2'           DVAERRORE
022600941216     C                   OTHER
022700950607     C                   MOVE      '0'           DVAERRORE
022800941216     C* aggancia ANSOG00F
022900950607     C                   MOVE      CLNSOGG       SOGSOGG
023000941216     C                   EXSR      CHASOG
023100941216     C* aggancia ANIND00F
023200950607   3BC     DVAERRORE     IFEQ      '0'
023300950607     C                   MOVE      CLNSOGG       INDSOGG
023400950925     C*
023500950925     C                   IF        %addr(DVAtpind)<>*NULL and
023600950925     C                             %addr(DVAcdind)<>*NULL
023700950925     C                   MOVE      DVATPIND      INDTPIND
023800950925     C                   MOVE      dvaCDIND      INDCDIND
023900950925     C                   else
024000950925     C                   MOVE      clnTPIND      INDTPIND
024100950925     C                   MOVE      clnCDIND      INDCDIND
024200950925     C                   endif
024300950925     c*
024400941216     C                   EXSR      CHAIND
024500941216   3EC                   ENDIF
024600941216     C*
024700941216   2EC                   ENDSL
024800941216     C*
024900941216     C                   ENDSR
025000941216     C/EJECT
025100941216     C************************************************************
025200941216     C* AGGANCIO A FILE ANSOG00F
025300941216     C************************************************************
025400941216     C     CHASOG        BEGSR
025500941216     C*
025600950607     C     K1SOG01       CHAIN     ANSOG01L                           2122
025700941216     C* se il rcd da gestire non viene trovato
025800941216     C* oppure è allocato da altro pgm
025900941216     C* ritorno al pgm chiamante avvertendolo
026000941216   3SC                   SELECT
026100941216     C     *IN21         WHENEQ    *ON
026200950607     C                   MOVE      '1'           DVAERRORE
026300941216     C     *IN22         WHENEQ    *ON
026400950607     C                   MOVE      '2'           DVAERRORE
026500941216     C                   OTHER
026600950607     C                   MOVE      '0'           DVAERRORE
026601180313>>>>>C                   IF        sogPartIva = '$$'
026602180313>>>>>C                   CLEAR                   sogPartIva
026603180313>>>>>C                   ENDIF
026604180313>>>>>C                   IF        sogCdFisc = '$$'
026605180313>>>>>C                   CLEAR                   sogCdFisc
026606180313>>>>>C                   ENDIF
026700941216     C                   ENDSL
026800941216     C*
026900941216     C                   ENDSR
027000941216     C/EJECT
027100941216     C************************************************************
027200941216     C* AGGANCIO A FILE ANIND00F
027300941216     C************************************************************
027400941216     C     CHAIND        BEGSR
027500941216     C*
027600950607     C     K3IND01       CHAIN     ANIND01L                           2122
027700941216     C* se il rcd da gestire non viene trovato
027800941216     C* oppure è allocato da altro pgm
027900941216     C* ritorno al pgm chiamante avvertendolo
028000941216     C                   SELECT
028100950607     C     *IN21         WHENEQ    *ON
028200950607     C                   MOVE      '1'           DVAERRORE
028300941216     C     *IN22         WHENEQ    *ON
028400950607     C                   MOVE      '2'           DVAERRORE
028500941216     C                   OTHER
028600950607     C                   MOVE      '0'           DVAERRORE
028601180313>>>>>** I clienti di San Marino sono codificati come se fossero dei romagnoli.
028602180313>>>>>C                   IF        %SUBST(indCap:1:4) = '4789' AND
028603180313>>>>>C                             indStato = 'IT'
028604180313>>>>>C                   EVAL      indStato = 'SM'
028605180313>>>>>C                   CLEAR                   indProv
028606180313>>>>>C                   ENDIF
028700941216     C                   ENDSL
028800941216     C*
028900941216     C                   ENDSR
029000941216     C/EJECT
029100100723C2163C************************************************************
029200100723  "  C* AGGANCIO A FILE ANSO100F
029300100723  "  C************************************************************
029400100723C2163C     CHASO1        BEGSR
029500100723     C*
029600100723     C     K1SO101       CHAIN     ANSO101L                           2122
029700100723     C* se il rcd da gestire non viene trovato
029800100723     C* oppure è allocato da altro pgm
029900100723     C* ritorno al pgm chiamante avvertendolo
030000100723     C                   SELECT
030100100723     C     *IN21         WHENEQ    *ON
030200100723     C                   MOVE      '1'           DVAERRORE
030300100723     C     *IN22         WHENEQ    *ON
030400100723     C                   MOVE      '2'           DVAERRORE
030500100723     C                   OTHER
030600100723     C                   MOVE      '0'           DVAERRORE
030700100723     C                   ENDSL
030800100723     C*
030900100723     C                   ENDSR
031000100723C2163C/EJECT
031100941216     C************************************************************
031200950607     C* RIEMPIE CAMPI PER DVACLI
031300941216     C************************************************************
031400950607     C     RIECLI        BEGSR
031500950607     C* ANSOG
031600950607     C*
031700950607     C                   MOVE      SOGSOGG       DVCSOGG
031800950607     C                   MOVE      SOGDTENTR     DVCDTENTR
031900950607     C                   MOVE      SOGDTULMO     DVCdtulmo
032000950607     C                   MOVE      SOGDTINVL     DVCdtinvl
032100950607     C                   MOVE      SOGDTFIVL     DVCdtfivl
032200950607     C                   MOVE      SOGDTNASC     DVCdtnasc
032300950607     C                   MOVE      SOGDES        DVCdes
032400950607     C                   MOVE      SOGDESAGG     DVCdesagg
032500950607     C                   MOVE      SOGCDFISC     DVCcdfisc
032600950607     C                   MOVE      SOGPARTIVA    DVCpartiva
032700950607     C                   MOVE      SOGFRCPIVA    DVCfrcpiva
032800950607     C                   MOVE      SOGISCCIAA    DVCiscciaa
032900950607     C                   MOVE      SOGSEX        DVCsex
033000950607     C                   MOVE      SOGNATGIUR    DVCnatgiur
033100950607     C                   MOVE      SOGCODPREC    DVCcodprec
033200950607     C                   MOVE      SOGABI        DVCabi
033300950607     C                   MOVE      SOGCLFSOG     dvcclfsog
033400950607     C*
033500950607     C* ANIND
033600950607     C*
033700950607     C                   MOVE      INDSOGG       DVCSOGGI
033800950607     C                   MOVE      INDTPIND      DVCTPIND
033900950607     C                   MOVE      INDCDIND      DVCCDIND
034000950607     C                   MOVE      INDDTULMO     DVCDTULMOI
034100950607     C                   MOVE      INDDTINVL     DVCDTINVLI
034200950607     C                   MOVE      INDDTFIVL     DVCDTFIVLI
034300950607     C                   MOVE      INDDESFIL     DVCDESFIL
034400950607     C                   MOVE      INDDESAGG     DVCDESAGGI
034500950607     C                   MOVE      INDINDRIZ     DVCINDRIZ
034600950607     C                   MOVE      INDLOCALIT    DVCLOCALIT
034700950607     C                   MOVE      INDCAP        DVCCAP
034800950607     C                   MOVE      INDPROV       DVCPROV
034900950607     C                   MOVE      INDSTATO      DVCSTATO
035000950607     C                   MOVE      INDCAB        DVCCAB
035100950607     C                   MOVE      INDLINGUA     DVCLINGUA
035200950607     C                   MOVE      INDTELEX      DVCTELEX
035300950607     C                   MOVE      INDTELEFON    DVCTELEFON
035400950607     C                   MOVE      INDTELEFAX    DVCTELEFAX
035500950607     C*
035600950607     C* ANCLN
035700950607     C                   MOVE      CLNSOCIETA    DVCSOCIETA
035800950607     C                   MOVE      CLNUNITA      DVCUNITA
035900950607     C                   MOVE      CLNKCC        DVCKCC
036000950607     C                   MOVE      CLNKSC        DVCKSC
036100950607     C                   MOVE      CLNSOGG       DVCSOGGC
036200950607     C                   MOVE      CLNTPIND      DVCTPINDC
036300950607     C                   MOVE      CLNCDIND      DVCCDINDC
036400950607     C                   MOVE      CLNDESBREV    DVCDESBREV
036500950607     C                   MOVE      CLNLINEAV     DVCLINEAV
036600950607     C                   MOVE      CLNFILIALE    DVCFILIALE
036700950607     C                   MOVE      CLNDTINRPC    DVCDTINRPC
036800950607     C                   MOVE      CLNUTENTE     DVCUTENTE
036900950607     C                   MOVE      CLNDTULMO     DVCDTULMOC
037000950607     C                   MOVE      CLNCLIPREC    DVCCLIPREC
037100950607     C                   MOVE      CLNFORCORR    DVCFORCORR
037200950607     C                   MOVE      CLNCLIFATT    DVCCLIFATT
037300950607     C                   MOVE      CLNADDSPBA    DVCADDSPBA
037400950607     C                   MOVE      CLNSPEBOLL    DVCSPEBOLL
037500950607     C                   MOVE      CLNNRCPFAT    DVCNRCPFAT
037600950607     C                   MOVE      CLNIMBUSTA    DVCIMBUSTA
037700950607     C                   MOVE      CLNFIDOSTA    DVCFIDOSTA
037800950607     C                   MOVE      CLNFIDOASS    DVCFIDOASS
037900950607     C                   MOVE      CLNDTFIST     DVCDTFIST
038000950607     C                   MOVE      CLNDTFIAS     DVCDTFIAS
038100950607     C                   MOVE      CLNCLIASSO    DVCCLIASSO
038200950607     C                   MOVE      CLNGGRCVAG    DVCGGRCVAG
038300950607     C                   MOVE      CLNCTRCONT    DVCCTRCONT
038400950607     C                   MOVE      CLNCITPROV    DVCCITPROV
038500950607     C                   MOVE      CLNSTATIST    DVCSTATIST
038600950607     C                   MOVE      CLNDTFIRPC    DVCDTFIRPC
038700950607     C                   MOVE      CLNCLASABC    DVCCLASABC
038800950607     C                   MOVE      CLNTPTESTO    DVCTPTESTO
038900950607     C                   MOVE      CLNMAG        DVCMAG
039000950607     C                   MOVE      CLNADDIVA     DVCADDIVA
039100950607     C                   MOVE      CLNFREQFAT    DVCFREQFAT
039200950607     C                   MOVE      CLNRAGGBOL    DVCRAGGBOL
039300950607     C                   MOVE      CLNRAGGART    DVCRAGGART
039400950607     C                   MOVE      CLNMDSPED     DVCMDSPED
039500950607     C                   MOVE      CLNRESA       DVCRESA
039600950607     C                   MOVE      CLNSTASCOF    DVCSTASCOF
039700950607     C                   MOVE      CLNDETPREZ    DVCDETPREZ
039800950607     C                   MOVE      CLNDTINRPL    DVCDTINRPL
039900950607     C                   MOVE      CLNDTFIRPL    DVCDTFIRPL
040000950607     C                   MOVE      CLNLISTINO    DVCLISTINO
040100950607     C                   MOVE      CLNDIVISA     DVCDIVISA
040200950607     C                   MOVE      CLNCAPODIS    DVCCAPODIS
040300950607     C                   MOVE      CLNFUNZRIO    DVCFUNZRIO
040400950607     C                   MOVE      CLNAGENTE1    DVCAGENTE1
040500950607     C                   MOVE      CLNAGENTE2    DVCAGENTE2
040600950607     C                   MOVE      CLNAGENTE3    DVCAGENTE3
040700950607     C                   MOVE      CLNPRIOEVA    DVCPRIOEVA
040800950607     C                   MOVE      CLNBKGSCLI    DVCBKGSCLI
040900950607     C                   MOVE      CLNRAGGSCO    DVCRAGGSCO
041000950607     C                   MOVE      CLNRAGGPRV    DVCRAGGPRV
041100950607     C                   MOVE      CLNLORDNET    DVCLORDNET
041200950607     C                   MOVE      CLNCONDPAG    DVCCONDPAG
041300950607     C                   MOVE      CLNSCOCASS    DVCSCOCASS
041400950607     C                   MOVE      CLNPERCAD1    DVCPERCAD1
041500950607     C                   MOVE      CLNPERCAD2    DVCPERCAD2
041600950607     C                   MOVE      CLNCONFORD    DVCCONFORD
041700950607     C                   MOVE      CLNRAGGVEN    DVCRAGGVEN
041800950607     C                   MOVE      CLNSTAPREZ    DVCSTAPREZ
041900950607     C                   MOVE      CLNCDIVA      DVCCDIVA
042000950607     C                   MOVE      CLNTPINDCM    DVCTPINDCM
042100950607     C                   MOVE      CLNSCOPAG1    DVCSCOPAG1
042200950607     C                   MOVE      CLNSCOPAG2    DVCSCOPAG2
042300950607     C                   MOVE      CLNGGINPA1    DVCGGINPA1
042400950607     C                   MOVE      CLNGGINPA2    DVCGGINPA2
042500950607     C                   MOVE      CLNGGINPA3    DVCGGINPA3
042600950607     C                   MOVE      CLNSESSTRA    DVCSESSTRA
042700950607     C                   MOVE      CLNSESSRIC    DVCSESSRIC
042800950607     C                   MOVE      CLNTPPVEND    DVCTPPVEND
042900950607     C                   MOVE      CLNTPCAUZ     DVCTPCAUZ
043000950607     C                   MOVE      CLNACCONTO    DVCACCONTO
043100950607     C                   MOVE      CLNCLICESS    DVCCLICESS
043200950607     C                   MOVE      CLNCLISOLL    DVCCLISOLL
043300950607     C                   MOVE      CLNADDSOLL    DVCADDSOLL
043400950607     C                   MOVE      CLNCONTRAS    DVCCONTRAS
043500950607     C                   MOVE      CLNADDPROD    DVCADDPROD
043600950607     C                   MOVE      CLNRAGGCON    DVCRAGGCON
043700950607     C                   MOVE      CLNFLG1       DVCFLG1
043800950607     C                   MOVE      CLNFLG2       DVCFLG2
043900950607     C                   MOVE      CLNFLG3       DVCFLG3
044000950607     C                   MOVE      CLNFLG4       DVCFLG4
044100950607     C                   MOVE      CLNFLG5       DVCFLG5
044200950607     C                   MOVE      CLNFLG6       DVCFLG6
044300950607     C*
044400941216     C                   ENDSR
044500941216     C*
044600950607     C/EJECT
044700950607     C************************************************************
044800950607     C* ROUTINE PER REPERIRE DATI PER DS DVAFOR
044900950607     C************************************************************
045000950607     C     DATFOR        BEGSR
045100950607     C*
045200950607     C                   MOVE      DVASOCIETA    FRNSOCIETA
045300950607     C                   MOVE      WRKCODICE     FRNKSC
045400950607     C                   MOVE      WRKFILIALE    FRNFILIALE
045500950607     C     K3FRN01       CHAIN     ANFRN01L                           2122
045600950607     C*
045700950607     C* se il rcd da gestire non viene trovato
045800950607     C* oppure è allocato da altro pgm
045900950607     C* ritorno al pgm chiamante avvertendolo
046000950607   2SC                   SELECT
046100950607     C     *IN21         WHENEQ    *ON
046200950607     C                   MOVE      '1'           DVAERRORE
046300950607     C     *IN22         WHENEQ    *ON
046400950607     C                   MOVE      '2'           DVAERRORE
046500950607     C                   OTHER
046600950607     C                   MOVE      '0'           DVAERRORE
046700950607     C* aggancia ANSOG00F
046800950607     C                   MOVE      FRNSOGG       SOGSOGG
046900950607     C                   EXSR      CHASOG
047000950607     C* aggancia ANIND00F
047100950607   3BC     DVAERRORE     IFEQ      '0'
047200950607     C                   MOVE      FRNSOGG       INDSOGG
047300950925     C*
047400950925     C                   IF        %addr(DVAtpind)<>*NULL and
047500950925     C                             %addr(DVAcdind)<>*NULL
047600950925     C                   MOVE      DVATPIND      INDTPIND
047700950925     C                   MOVE      dvaCDIND      INDCDIND
047800950925     C                   else
047900950925     C                   MOVE      frnTPIND      INDTPIND
048000950925     C                   MOVE      frnCDIND      INDCDIND
048100950925     C                   endif
048200950607     C                   EXSR      CHAIND
048300950607     C* aggancia ANAPE00F  (che può anche non esserci)
048400950607   3BC     DVAERRORE     IFEQ      '0'
048500950607     C                   MOVE      FRNSOCIETA    APESOCIETA
048600950607     C                   MOVE      FRNKCC        APEKCC
048700950607     C                   MOVE      FRNKSC        APEKSC
048800950607     C     K3APE01       CHAIN     ANAPE01L                           21
048900950607     C     *IN21         IFEQ      *OFF
049000950607     C                   EXSR      RIEAPE
049100950607   3EC                   ENDIF
049200950607   3EC                   ENDIF
049300950607   3EC                   ENDIF
049400950607     C*
049500950607   2EC                   ENDSL
049600950607     C*
049700950607     C                   ENDSR
049800950607     C/EJECT
049900950607     C************************************************************
050000950607     C* RIEMPIE CAMPI PER DVAFOR
050100950607     C************************************************************
050200950607     C     RIEFOR        BEGSR
050300950607     C*
050400950607     C* ANSOG
050500950607     C*
050600950607     C                   MOVE      SOGSOGG       DVFSOGG
050700950607     C                   MOVE      SOGDTENTR     DVFDTENTR
050800950607     C                   MOVE      SOGDTULMO     DVFdtulmo
050900950607     C                   MOVE      SOGDTINVL     DVFdtinvl
051000950607     C                   MOVE      SOGDTFIVL     DVFdtfivl
051100950607     C                   MOVE      SOGDTNASC     DVFdtnasc
051200950607     C                   MOVE      SOGDES        DVFdes
051300950607     C                   MOVE      SOGDESAGG     DVFdesagg
051400950607     C                   MOVE      SOGCDFISC     DVFcdfisc
051500950607     C                   MOVE      SOGPARTIVA    DVFpartiva
051600950607     C                   MOVE      SOGFRCPIVA    DVFfrcpiva
051700950607     C                   MOVE      SOGISCCIAA    DVFiscciaa
051800950607     C                   MOVE      SOGSEX        DVFsex
051900950607     C                   MOVE      SOGNATGIUR    DVFnatgiur
052000950607     C                   MOVE      SOGCODPREC    DVFcodprec
052100950607     C                   MOVE      SOGABI        DVFabi
052200950607     C                   MOVE      SOGCLFSOG     dvFclfsog
052300950607     C*
052400950607     C* ANIND
052500950607     C*
052600950607     C                   MOVE      INDSOGG       DVFSOGGI
052700950607     C                   MOVE      INDTPIND      DVFTPIND
052800950607     C                   MOVE      INDCDIND      DVFCDIND
052900950607     C                   MOVE      INDDTULMO     DVFDTULMOI
053000950607     C                   MOVE      INDDTINVL     DVFDTINVLI
053100950607     C                   MOVE      INDDTFIVL     DVFDTFIVLI
053200950607     C                   MOVE      INDDESFIL     DVFDESFIL
053300950607     C                   MOVE      INDDESAGG     DVFDESAGGI
053400950607     C                   MOVE      INDINDRIZ     DVFINDRIZ
053500950607     C                   MOVE      INDLOCALIT    DVFLOCALIT
053600950607     C                   MOVE      INDCAP        DVFCAP
053700950607     C                   MOVE      INDPROV       DVFPROV
053800950607     C                   MOVE      INDSTATO      DVFSTATO
053900950607     C                   MOVE      INDCAB        DVFCAB
054000950607     C                   MOVE      INDLINGUA     DVFLINGUA
054100950607     C                   MOVE      INDTELEX      DVFTELEX
054200950607     C                   MOVE      INDTELEFON    DVFTELEFON
054300950607     C                   MOVE      INDTELEFAX    DVFTELEFAX
054400950607     C*
054500950607     C* ANFRN
054600950607     C*
054700950607     C                   MOVE      FRNSOCIETA    DVFSOCIETF
054800950607     C                   MOVE      FRNUNITA      DVFUNITA
054900950607     C                   MOVE      FRNKCC        DVFKCC
055000950607     C                   MOVE      FRNKSC        DVFKSC
055100950607     C                   MOVE      FRNFILIALE    DVFFILIALE
055200950607     C                   MOVE      FRNSOGG       DVFSOGGF
055300950607     C                   MOVE      FRNTPIND      DVFTPINDF
055400950607     C                   MOVE      FRNCDIND      DVFCDINDF
055500950607     C                   MOVE      FRNDTULMO     DVFDTULMOF
055600950607     C                   MOVE      FRNUTENTE     DVFUTENTE
055700950607     C                   MOVE      FRNDTINRPF    DVFDTINRPF
055800950607     C                   MOVE      FRNDTFIRPF    DVFDTFIRPF
055900950607     C                   MOVE      FRNDESBREV    DVFDESBREV
056000950607     C                   MOVE      FRNGRUMERC    DVFGRUMERC
056100950607     C                   MOVE      FRNSETMERC    DVFSETMERC
056200950607     C                   MOVE      FRNFORALT     DVFFORALT
056300950607     C                   MOVE      FRNLISTINO    DVFLISTINO
056400950607     C                   MOVE      FRNDIVISA     DVFDIVISA
056500950607     C                   move      FRNCONDCON    DVFCONDCON
056600950607     C                   move      FRNQUALFOR    DVFQUALFOR
056700950607     C                   move      FRNVOLAFF     DVFVOLAFF
056800950607     C                   move      FRNCDIVA      DVFCDIVA
056900950607     C                   move      FRNCOLLOBB    DVFCOLLOBB
057000950607     C                   move      FRNTPCOLL     DVFTPCOLL
057100950607     C                   move      FRNFORLIST    DVFFORLIST
057200950607     C                   move      FRNFORFATT    DVFFORFATT
057300950607     C                   move      FRNFORPOT     DVFFORPOT
057400950607     C                   move      FRNREPPRZL    DVFREPPRZL
057500950607     C                   move      FRNFORBLO     DVFFORBLO
057600950607     C                   move      FRNLISFIL     DVFLISFIL
057700950607     C                   move      FRNABILPRI    DVFABILPRI
057800950607     C                   move      FRNMDCONS     DVFMDCONS
057900950607     C                   move      FRNMAG        DVFMAG
058000950607     C                   move      FRNCLICORR    DVFCLICORR
058100950607     C                   move      FRNFORPREC    DVFFORPREC
058200950607     C                   move      FRNSCOPAG     DVFSCOPAG
058300950607     C                   move      FRNSCOPAG2    DVFSCOPAG2
058400950607     C                   move      FRNGGINPA1    DVFGGINPA1
058500950607     C                   move      FRNGGINPA2    DVFGGINPA2
058600950607     C                   move      FRNGGINPA3    DVFGGINPA3
058700950607     C                   move      FRNSESSTRA    DVFSESSTRA
058800950607     C                   move      FRNSESSRIC    DVFSESSRIC
058900950607     C                   move      FRNCONDPAG    DVFCONDPAG
059000950607     C*
059100950607     C                   ENDSR
059200950607     C/EJECT
059300950607     C************************************************************
059400950607     C* RIEMPIE CAMPI DI ANAPE SE PRESENTE
059500950607     C************************************************************
059600950607     C     RIEAPE        BEGSR
059700950607     C*
059800950607     C*  ANAPE
059900950607     C*
060000950607     C                   move      APESOCIETA    DVFSOCIETA
060100950607     C                   move      APEKCC        DVFKCCA
060200950607     C                   move      APEKSC        DVFKSCA
060300950607     C                   move      APETRIBUTO    DVFTRIBUTO
060400950607     C                   move      APEVARTRIB    DVFVARTRIB
060500950607     C                   move      APETPTESTO    DVFTPTESTO
060600950607     C                   move      APETPIND      DVFTPINDA
060700950607     C                   move      APECDIND      DVFCDINDA
060800950607     C                   move      APELOCALIT    DVFLOCALIA
060900950607     C                   move      APEPROV       DVFPROVA
061000950607     C                   move      APESTATO      DVFSTATOA
061100950607     C                   move      APECDESMIN    DVFCDESMIN
061200950607     C                   move      APECDFISC     DVFCDFISCA
061300950920     C                   move      APECOGNOME    DVFCOGNOME
061400950920     C                   move      APENOME       DVFNOME
061500950920     C                   move      APEDTCONT     DVFDTCONT
061600981222     C                   move      APETINPS      DVFTINPS
061700981222     C                   move      APEPINPS      DVFPINPS
061800981222     C                   move      APECLFSOG     DVFCLFSOA
061900981222     C                   move      APEFLAG       DVFFLAG
062000120904A1357C                   move      APEDTFINE     DVFDTFINE
062100950607     C*
062200950607     C                   ENDSR
062300950607     C*
062400950607     C/EJECT
062500950607     C************************************************************
062600950607     C* ROUTINE PER REPERIRE DATI PER DS DVASOG
062700950607     C************************************************************
062800950607     C     DATSOG        BEGSR
062900950607     C*
063000950925     C                   MOVE      WRKSOGG       SOGSOGG
063100950925     C     K1SOG01       CHAIN     ANSOG01L                           2122
063200950607     C*
063300950607     C* se il rcd da gestire non viene trovato
063400950607     C* oppure è allocato da altro pgm
063500950607     C* ritorno al pgm chiamante avvertendolo
063600950607   2SC                   SELECT
063700950607     C     *IN21         WHENEQ    *ON
063800950607     C                   MOVE      '1'           DVAERRORE
063900950607     C     *IN22         WHENEQ    *ON
064000950607     C                   MOVE      '2'           DVAERRORE
064100950607     C                   OTHER
064200950607     C                   MOVE      '0'           DVAERRORE
064201180313>>>>>C                   IF        sogPartIva = '$$'
064202180313>>>>>C                   CLEAR                   sogPartIva
064203180313>>>>>C                   ENDIF
064204180313>>>>>C                   IF        sogCdFisc = '$$'
064205180313>>>>>C                   CLEAR                   sogCdFisc
064206180313>>>>>C                   ENDIF
064300950925     C* aggancia ANind00F
064400950925     C                   MOVE      SOGSOGG       INDSOGG
064500950925   3BC     DVAERRORE     IFEQ      '0'
064600950925     C*
064700950925     C                   IF        %addr(DVAtpind)<>*NULL and
064800950925     C                             %addr(DVAcdind)<>*NULL
064900950925     C                   MOVE      DVATPIND      INDTPIND
065000950925     C                   MOVE      dvaCDIND      INDCDIND
065100950925     C                   else
065200950925     C                   MOVE      *blanks       INDTPIND
065300950925     C                   MOVE      *blanks       INDCDIND
065400950925     C                   endif
065500950925     C                   EXSR      CHAIND
065600950925     C                   ENDIF
065700950607     C*
065800100723C2163C* aggancia ANSO100F
065900100726  "  C                   IF        DVAERRORE='0' and DVASTRUTT='ANSO1'
066000100726  "  C                   MOVE      SOGSOGG       SO1SOGG
066100100723  "  C                   EXSR      CHASO1
066200100723  "  C                   ENDIF
066300100723C2163C*
066400950607   2EC                   ENDSL
066500950607     C*
066600950607     C                   ENDSR
066700950607     C/EJECT
066800950607     C************************************************************
066900950607     C* RIEMPIE CAMPI PER DVASOG
067000950607     C************************************************************
067100950607     C     RIESOG        BEGSR
067200950607     C*
067300950607     C* ANSOG
067400950607     C*
067500950607     C                   MOVE      SOGSOGG       DVSSOGG
067600950607     C                   MOVE      SOGDTENTR     DVSDTENTR
067700950607     C                   MOVE      SOGDTULMO     DVSdtulmo
067800950607     C                   MOVE      SOGDTINVL     DVSdtinvl
067900950607     C                   MOVE      SOGDTFIVL     DVSdtfivl
068000950607     C                   MOVE      SOGDTNASC     DVSdtnasc
068100950607     C                   MOVE      SOGDES        DVSdes
068200950607     C                   MOVE      SOGDESAGG     DVSdesagg
068300950607     C                   MOVE      SOGCDFISC     DVScdfisc
068400950607     C                   MOVE      SOGPARTIVA    DVSpartiva
068500950607     C                   MOVE      SOGFRCPIVA    DVSfrcpiva
068600950607     C                   MOVE      SOGISCCIAA    DVSiscciaa
068700950607     C                   MOVE      SOGSEX        DVSsex
068800950607     C                   MOVE      SOGNATGIUR    DVSnatgiur
068900950607     C                   MOVE      SOGCODPREC    DVScodprec
069000950607     C                   MOVE      SOGABI        DVSabi
069100950607     C                   MOVE      SOGCLFSOG     dvSclfsog
069200950607     C*
069300950607     C* ANIND
069400950607     C*
069500950607     C                   MOVE      INDSOGG       DVSSOGGI
069600950607     C                   MOVE      INDTPIND      DVSTPIND
069700950607     C                   MOVE      INDCDIND      DVSCDIND
069800950607     C                   MOVE      INDDTULMO     DVSDTULMOI
069900950607     C                   MOVE      INDDTINVL     DVSDTINVLI
070000950607     C                   MOVE      INDDTFIVL     DVSDTFIVLI
070100950607     C                   MOVE      INDDESFIL     DVSDESFIL
070200950607     C                   MOVE      INDDESAGG     DVSDESAGGI
070300950607     C                   MOVE      INDINDRIZ     DVSINDRIZ
070400950607     C                   MOVE      INDLOCALIT    DVSLOCALIT
070500950607     C                   MOVE      INDCAP        DVSCAP
070600950607     C                   MOVE      INDPROV       DVSPROV
070700950607     C                   MOVE      INDSTATO      DVSSTATO
070800950607     C                   MOVE      INDCAB        DVSCAB
070900950607     C                   MOVE      INDLINGUA     DVSLINGUA
071000950607     C                   MOVE      INDTELEX      DVSTELEX
071100950607     C                   MOVE      INDTELEFON    DVSTELEFON
071200950607     C                   MOVE      INDTELEFAX    DVSTELEFAX
071300950607     C*
071400950607     C                   ENDSR
071500950607     C*
071600941216     C/EJECT
071700941216     C************************************************************
071800941216     C* INIZIALIZZAZIONE VARIABILI
071900941216     C************************************************************
072000941216     C     INZVAR        BEGSR
072100941216     C*
072200941216     C* Reperimento parametri
072300941216     C*
072400941216     C     *ENTRY        PLIST
072500950612     C                   PARM                    DVARIC
072600950612     C                   PARM                    DVASOCIETA
072700950612     C                   PARM                    DVAUNITA
072800950615     C                   PARM                    DVASTRUTT
072900950612     C                   PARM                    DVADTRIF
073000950612     C                   PARM                    DVALENOUT
073100950612     C                   PARM                    DVASNATURA
073200950612     C                   PARM                    DVACODICE
073300950612     C                   PARM                    DVALINEAV
073400950612     C                   PARM                    DVAFILIALE
073500950612     C                   PARM                    DVASOGG
073600950612     C                   PARM                    DVATPIND
073700950612     C                   PARM                    DVACDIND
073800950615     C                   PARM                    DVAERRORE
073900950615     C                   PARM                    DVAOUTPUT
074000980722     C*
074100980722     C* apro ANSOG
074200980722     C                   IF        OpenSOG = *off
074300980722     C                   OPEN      ANSOG01L
074400980722     C                   EVAL      OpenSOG = *on
074500980722     C                   ENDIF
074600980722     C*
074700980722     C* apro ANIND
074800980722     C                   IF        OpenIND = *off
074900980722     C                   OPEN      ANIND01L
075000980722     C                   EVAL      OpenIND = *on
075100980722     C                   ENDIF
075200980722     C*
075300980722     C* apro ANCLN
075400980722     C                   IF        OpenCLN = *off
075500980722     C                   OPEN      ANCLN01L
075600980722     C                   EVAL      OpenCLN = *on
075700980722     C                   ENDIF
075800980722     C*
075900980722     C* apro ANFRN
076000980722     C                   IF        OpenFRN = *off
076100980722     C                   OPEN      ANFRN01L
076200980722     C                   EVAL      OpenFRN = *on
076300980722     C                   ENDIF
076400980722     C*
076500980722     C* apro ANAPE
076600980722     C                   IF        OpenAPE = *off
076700980722     C                   OPEN      ANAPE01L
076800980722     C                   EVAL      OpenAPE = *on
076900980722     C                   ENDIF
077000100723C2163C*
077100100723  "  C* apro ANAPE
077200100723  "  C                   IF        OpenSO1 = *off
077300100723  "  C                   OPEN      ANSO101L
077400100723  "  C                   EVAL      OpenSO1 = *on
077500100723C2163C                   ENDIF
077600980722     C*
077700950711     C*                  CLEAR                   DVAOUTPUT
077800950616     C                   CLEAR                   DVAcli
077900950616     C                   CLEAR                   DVAfor
078000950616     C                   CLEAR                   DVAsog
078100950616     C                   CLEAR                   $OPE              3
078200950615     C                   MOVE      *ON           DVAERRORE
078300950607     C*
078400961008     C*  Reperisco dati società solo se diversa da quella già impostata
078500961008     C*
078600961008     c                   if        xscsoc <> DVASOCIETA or
078700961008     c                             xscsoc = *blanks
078800981102     C                   MOVEL     'SOC002'      TIPXSC
078900961008     C                   MOVEL     DvaSocieta    SOCXSC
079000961008     C                   EXSR      REPSOC
079100961008     C     RTNXSC        IFNE      '1'
079200961008     C                   MOVEL     XSOCDS        SOC001
079300961008     C                   ELSE
079400961008     C                   EXSR      ENDPGM
079500961008     C                   ENDIF
079600961008     C                   endif
079700950607     C
079800950607     C*
079900950607     C* individuazione del reperimento dati
080000941216     C*
080100950615     C                   IF        %addr(DVACODICE)<>*NULL and
080200950616     C                             dvasnatura ='C'
080300950616     C                   MOVE      'CLI'         $OPE
080400950607     C                   MOVE      DVACODICE     WRKCODICE
080500950607     C                   MOVE      DVASNATURA    WRKSNATURA
080600950607     C                   MOVE      DVAFILIALE    WRKFILIALE
080700961008     c* Se linea di vendita = *blanks e nel flag della società si indica
080800961008     c* che unità e linea di vendita coincidono, nel campo wrklineav
080900961008     c* metto l'unità
081000961008     C                   IF        DVALINEAV = *blanks
081100961008     C                             and fgO5 = *on
081200980907     C*controllo che DVAUNITA sia lungo al max 3
081300980907     C                   move      dvaunita      cinque
081400980907     C                   if        cinque = *blanks
081500980907     C                   MOVEL     DVAUNITA      WRKLINEAV
081600980907     C                   ELSE
081700980907     C                   move      *blanks       wrklineav
081800980907     C                   ENDIF
081900980907     C*
082000980907     C*lo stesso controllo può essere fatto usando la function %len,
082100980907     C*ma per fare questo è necessario un compilatore almeno nella
082200980907     C*versione 3.7. In quel caso sarà possibile utilizzare il codice
082300980907     C*seguente:
082400980907     C*                  IF        %LEN(%TRIMR(DVAUNITA)) <=3
082500980907     C*                  MOVEL     DVAUNITA      WRKLINEAV
082600980907     C*                  ELSE
082700980907     C*                  MOVE      *BLANKS       WRKLINEAV
082800980907     C*                  ENDIF
082900980907     C*
083000961008     C                   ELSE
083100961008     C                   MOVE      DVALINEAV     WRKLINEAV
083200961008     C                   ENDIF
083300950615     C                   MOVE      *OFF          DVAERRORE
083400950925     C                   ENDIF
083500950607     C*
083600950615     C                   if        %addr(DVACODICE)<>*NULL and
083700950616     C                             dvasnatura ='F'
083800950616     C                   MOVE      'FOR'         $OPE
083900950607     C                   MOVE      DVACODICE     WRKCODICE
084000950607     C                   MOVE      DVASNATURA    WRKSNATURA
084100950607     C                   MOVE      DVAFILIALE    WRKFILIALE
084200950615     C                   MOVE      *OFF          DVAERRORE
084300950607     C                   ENDIF
084400950607     C*
084500950616     C                   if        %addr(DVASOGG)<>*NULL
084600950616     C                   MOVE      'SOG'         $OPE
084700950607     C                   MOVE      DVASOGG       WRKSOGG
084800950615     C                   MOVE      *OFF          DVAERRORE
084900950607     C                   ENDIF
085000950607     C*
085100950607     C* Se non imposta nessuna possibile ricerca, esco con errore
085200950607     C*
085300950615     C     DVAERRORE     IFEQ      *ON
085400950607     C                   EXSR      ENDPGM
085500950607     C                   ENDIF
085600950607     C*
085700941216     C                   ENDSR
085800961008      /EJECT
085900961008      ************************************************************
086000961008      * Reperimento dati società
086100961008      ************************************************************
086200961008     C     REPSOC        BEGSR
086300961008      *
086400961008     C                   CALLB     'XSOC'
086500961008     C                   PARM                    TIPXSC            6
086600961008     C                   PARM                    SOCXSC            3
086700961008     C                   PARM                    CDSXSC            9 0
086800961008     C                   PARM                    MODXSC            3
086900961008     C                   PARM      *BLANKS       RTNXSC            1
087000961008     C                   PARM                    XSOCDS
087100961008     C                   PARM                    KPJBA
087200961008      *
087300961008     C                   ENDSR
087400941216     C/EJECT
087500941216     C************************************************************
087600941216     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
087700941216     C************************************************************
087800941216     C     DEFCAM        BEGSR
087900941216     C*
088000941216     C* klist
088100941216     C*
088200950607     C* ANCLN01L
088300950607     C     K4CLN01       KLIST
088400950607     C                   KFLD                    CLNSOCIETA
088500950607     C                   KFLD                    CLNKSC
088600950607     C                   KFLD                    CLNFILIALE
088700950607     C                   KFLD                    CLNLINEAV
088800950607     C* ANFRN01L
088900950607     C     K3FRN01       KLIST
089000950607     C                   KFLD                    FRNSOCIETA
089100950607     C                   KFLD                    FRNKSC
089200950607     C                   KFLD                    FRNFILIALE
089300950607     C* ANSOG01L
089400950607     C     K1SOG01       KLIST
089500950607     C                   KFLD                    SOGSOGG
089600950607     C* ANIND01L
089700950607     C     K3IND01       KLIST
089800950607     C                   KFLD                    INDSOGG
089900950607     C                   KFLD                    INDTPIND
090000950607     C                   KFLD                    INDCDIND
090100950607     C* ANape01L
090200950607     C     K3APE01       KLIST
090300950607     C                   KFLD                    APESOCIETA
090400950607     C                   KFLD                    APEKCC
090500950607     C                   KFLD                    APEKSC
090600100723C2163C* ANSO101L
090700100723  "  C     K1SO101       KLIST
090800100723C2163C                   KFLD                    SO1SOGG
090900941216     C*
091000941216     C                   ENDSR
091100941216     C************************************************************
