000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200970123     H* TRTC83R4*----------------------------------------------------*
000300961111     H*    MANIFEST SPEDIZIONI PER  E.D.I. ESTERO
000400000000     H*--------------------------------------------------------------*
000500961122     FFNFGV01L  UF   E           K DISK
000600000505     FFNFGW01L  IF   E           K DISK
000700970124     FFNFGV46F  IF A E           K DISK
000800961122     F                                     RENAME(FNFGV000:FNFGV046)
000900970123     FAZORG01L  IF   E           K DISK
001000110510     FTntbe01l  IF   E           K DISK
001100050523     F****TREXP01L  UF   E           K DISK
001200050523     FWFMAN01L  UF   E           K DISK
001300990519     FFNANM04L  UF   E           K DISK
001400970114     F*--------- totali fv via fax e su carta
001500041021     FMANTOT    O    F   90        PRINTER USROPN
001600970114     F                                     FORMLEN(66)
001700970114     F                                     FORMOFL(44)
001800960529     D*--------------------------------------------------------------*
001900960529     D*  SCHIERE
002000960529     D*--------------------------------------------------------------*
002100021017     D CMD             S             65    DIM(2) CTDATA PERRCD(1)              OVRPRTF
002200970528     D TOT             S             66    DIM(11) CTDATA PERRCD(1)             RIGHE ST TOT.
002300970120     D DATX            S              1    DIM(32)                              SKI X CITTA' DATA
002400970528     D PES             S             10  1 DIM(20)                              TOT.PESO X LNA
002500970528     D TOC             S              7  0 DIM(20)                              TOT.COLLI LNA
002600050525     D tov             S             11  3 DIM(20)                              TOT.Volume LNA
002700970528     D LNA             S              3  0 DIM(20)                              LNA IN STAMPA
002800970528     D DES             S             10    DIM(20)                              DECOD. LNA
002900960529     D*--------------------------------------------------------------*
003000960529     D* DS
003100960529     D*--------------------------------------------------------------*
003200041021     D* ds per ind. email
003300041021     Dtrul44ds       E DS                  INZ
003400050913     D trtcm1ds      E DS
003500110510     D dvpobrt       e DS
003600041021      **
003700000405     D TRTC82DS      E DS
003800000405     D OG143         E DS
003900931229     D KPJBA         E DS
004000900511     D  LIBSYS                92    100
004100931229     D CNCR80        E DS
004200110511     D CNCR42        E DS
004300970123     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004400970113     D* LINEE DI ARRIVO  PER FNFGV00F
004500970113     D                 DS
004600970113     D  FGVLNA                 1      3  0
004700970113     D  FGVFFV                 4    243
004800970113     D  FGVFF2               244    450
004900000505     D  FGwFF3               451    690
005000000505     D  FGwFF4               691    900
005100000505     D  VUOTO                901    903
005200000505     D  FFV                    1    903
005300000505     D                                     DIM(301)                             LINEE DEL FV
005400970124     D                 DS
005500970124     D  AA                     1      4  0
005600970124     D  MM                     5      6  0
005700970124     D  GG                     7      8  0
005800970124     D  DATA                   1      8  0
005900020808      *
006000020808      *  Definizione variabili
006100060109     D Wdatagg         S              8  0 inz
006200060109     D Wdataoggi       S               D   DATFMT(*eur)
006300020808      *
006400020808     D PARDFV          S                   LIKE(D82DFV)
006500020808     D*
006600020808     D WLNA            S                   LIKE(D82LNP)
006700020808     D WTRC            S                   LIKE(TRETRC)
006800020808     D TOGPES          S                   LIKE(TRETOP)
006900020808     D TOGNCL          S                   LIKE(TRETOC)
007000050525     D TOGVlm          S                   LIKE(treTOV)
007100020808     D KCAA            S                   LIKE(ANMCAA)
007200020808     D DFV06           S                   LIKE(ANMDAO)
007300020808     D WPRV            S                   LIKE(D82PRO)
007400021017      *--------------------------------------------------------------*
007500021017      *    OVRPRTF x FAXIN
007600021017      *--------------------------------------------------------------*
007700021017     D                 DS
007800021017     D  OUTQPR                29     38
007900021017     D  CMDPRT                 1     65
008000021017      *--------------------------------------------------------------*
008100021017      * DEFINIZIONE COSTANTI x truloutq
008200021017      *--------------------------------------------------------------*
008300021017     D Return          C                   'R'
008400021017     D LR              C                   'L'
008500021017     D Eseguito        C                   '0'
008600021017     D Errore          C                   '1'
008700021017     D Generica        C                   '0'
008800021017     D POAmministr     C                   '1'
008900021017     D FaxIn           C                   '2'
009000021017     D FaxOut          C                   '3'
009100021017     D LetVettura      C                   '4'
009200021017      *
009300021017     D PrmFine         S              1    INZ(LR)
009400021017     D PrmTpOutQ       S              1
009500021017      *
009600021017     D PrmFilS         S              3S 0
009700021017     D PrmOutQ         S             10
009800021017     D PrmOutQLib      S             10
009900021017     D PrmEsito        S              1
010000021017      *
010100021017     D MyOutQ          S             10
010200021017     D MyOutQLib       S             10
010300021017      *
010400021017     D FaxInOutq       S             10
010500021017     D FaxInLibq       S             10
010600021017      *
010700021017     D FaxOutOutq      S             10
010800021017     D FaxOutLibq      S             10
010900960529     I*--------------------------------------------------------------*
011000960529     I* Ridefinisco campi file
011100960529     I*--------------------------------------------------------------*
011200941222     IFNFGV046
011300960529     I              FGVATB                      RGVATB
011400961115     I*
011500900523     C****************************************************************
011600950505     C*  RIEPILOGO INDICATORI
011700900523     C***************************************************************
011800970114     C* 10    - ON: STAMPO TOTALI FV ANCHE ANCHE PER FAX
011900961114     C* 20    - ON - ELABORAZIONE DEFINITIVA
012000970123     C* 22    - ON - FOGLIO VIAGGIO BIS
012100931229     C* 31/32 - DI COMODO
012200900523     C*****************************************************************
012300960529     C* CICLO PRINCIPALE
012400960529     C*****************************************************************
012500970115     C*
012600970114     C* CONTROLLO SE HO GIA' ELABORATO TUTTE LE LNA DEL FOGLIO VIAGGIO
012700970113     C*
012800970114     C  N20              SETOFF                                       32
012900020808     ***C   20D82NFV        CHAIN     FNFGV000                           3132
013000020808     ***C  N20D82NFV        CHAIN(N)  FNFGV000                           31
013100020808     C   20K02FGV01      chain     FNFGV000                           3132
013200020808     C  N20K02FGV01      chain(N)  FNFGV000                           31
013300970113    1C     *IN31         IFEQ      *OFF
013400970113     C     *IN32         ANDEQ     *OFF
013500970127     C     *IN20         ANDEQ     *ON
013600970127     C     FGVFCF        ANDEQ     ' '
013700970127     C     *IN31         OREQ      *OFF
013800970127     C     *IN32         ANDEQ     *OFF
013900970127     C     *IN20         ANDEQ     *OFF
014000970127     C*
014100020808     ***c     fgvnfv        chain     fnfgw01l                           39
014200020808     c     K02FGW01      chain     FNFGW01L                           39
014300000505     c                   if        *in39   or fgwatb<>' '
014400000505     c                   clear                   fgwff3
014500000505     c                   clear                   fgwff4
014600000505     c                   endif
014700060102     C                   CLEAR                   WNOK              1
014800970113     C                   Z-ADD     1             XX                3 0
014900970113    2C     FFV(XX)       DOWGT     *ZEROS
015000000505     C     XX            ANDLE     301
015100970113     C     WNOK          ANDEQ     *BLANKS
015200970113     C                   MOVE      FFV(XX)       WLNA
015300050523     C     KTRE          SETLL     WFMAN01L                               31
015400000405      * NON TROVATO RECORD WFILE RELATIVO A LNA SE LINEA DPD é OK ALTRIMENTI
015500000405      * --> NON HO ANCORA ELABORATO TUTTE LE LNA
015600970114    3C     *IN31         IFEQ      *OFF
015700060103      *----
015800060103      * ? ATTENZIONE:
015900060103      * ?   x DPD non deve + eseguire questo test poichè DPD è diventato
016000060103      * ?    come le altre linee.
016100060103      *----
016200060103      * ?  WNOK ->serve per far chiudere il foglio Viaggio solo se sono state
016300060103      * ?   elaborate tutte le linee presenti sul foglio viaggio e per fare ciò
016400060103      * ?   sono i records sul WFMAN a pilotare il gioco.
016500060103      * ? Esempio : elaborando il Manifest x Spagna e Portogallo 340 e 345
016600060103      * ?  al primo giro della az.TC83 sul WFMAN sarà presente solo il record 340
016700060103      * ?  quindi non deve chiudere il foglio viaggio mentre al secondo giro della
016800060103      * ?  az.TC83 sul WFMAN ci sono i 2 records del 340 e 345 quindi può procedere
016900060103      * ?  alla chiusura del Foglio viaggio e chiudere l'elaborazione del Manifest.
017000060104      * ?
017100060104     C                   if        Wdatagg >= 20060109
017200060104     C                   move      'N'           WNOK              1
017300060104     c                   else
017400060104      * ?  Fino al 8/1/2006 controlla se ci sono linee DPD
017500060104     C     wlna          chain     azorg
017600060104     c                   movel     ORGde3        OG143
017700060104     C                   IF        §OGntw <> 'DPD'
017800060103     C                   move      'N'           WNOK              1
017900060104    3C                   ENDIF
018000060104     c                   end
018100060103      *----
018200000405    3C                   ENDIF
018300000405      *
018400970113     C                   ADD       1             XX
018500970113    2C                   ENDDO
018600970123     C*
018700970113     C* HO ELABORATO TUTTE LE LNA DEL FOGLIO:
018800970218     C* 1) VEDO SE STAMPARE I TOTALI DEL FOGLIO VIAGGIO
018900970123     C* 2) SE ELABORAZIONE DEFINITIVA CHIUDO IL FOGLIO SCRIVO FNFGV46 X
019000970123     C*    SEDE E CANCELLO LE ANOMALIE 645
019100970123     C* 3) PULISCO IL FILE DI WORK
019200970123    2C     WNOK          IFEQ      *BLANKS
019300010509     C*
019400010509     C*  Se è stato chiamato solo per chiudere un foglio viaggio con sole linee
019500010509     C*   DPD non deve eseguire la parte di stampa ma deve solo aggiornare
019600010509     C*    l'archivio dei fogli viaggio.
019700010509     C     SoloDPD       Ifeq      *Blank
019800010509     C*
019900970123     C                   CLEAR                   TOGPES
020000970123     C                   CLEAR                   TOGNCL
020100050525     C                   clear                   togVlm
020200970528     C                   CLEAR                   PES
020300970528     C                   CLEAR                   TOC
020400050525     C                   CLEAR                   tov
020500970528     C                   CLEAR                   LNA
020600970528     C                   CLEAR                   DES
020700970528     C                   Z-ADD     1             II                2 0
020800050523     C     KTRE0         SETLL     WFMAN01L
020900050523     C     KTRE0         READE     wfman01L                               31
021000970123    3C     *IN31         DOWEQ     *OFF
021100970528     C                   ADD       TRETOP        TOGPES
021200970528     C                   ADD       TRETOC        TOGNCL
021300050525     C                   ADD       treTOV        TOGVlm
021400970528     C                   Z-ADD     TRETOP        PES(II)
021500970528     C                   Z-ADD     TRETOC        TOC(II)
021600050525     C                   z-add     treTOV        tov(II)
021700970528     C                   MOVE      TRELNA        LNA(II)
021800970528     C     TRELNA        CHAIN     AZORG01L                           31
021900970528     C  N31              MOVEL     ORGDES        DES(II)
022000970528     C                   ADD       1             II
022100050523     C     KTRE0         READE     wfman01L                               31
022200970123    3C                   ENDDO
022300970123     C*
022400970123    3C     TOGNCL        IFGT      *ZEROS
022500970123     C* RICHIAMO ROUTINE PER APRIRE FILE DI STAMPA TOTALI PER DOGANA
022600970123     C                   EXSR      OVRPRT
022700970123     C* DECODIFICHE PER STAMPA
022800970123     C*
022900970123     C                   MOVEL     *BLANKS       DESLNP           20
023000970123     C                   MOVEL     *BLANKS       INDLNP           20
023100970123     C                   Z-ADD     0             CAPLNP            5 0
023200970123     C                   MOVEL     *BLANKS       LOCLNP           20
023300970123     C                   MOVEL     *BLANKS       PROLNP            2
023400970123     C                   MOVEL     *BLANKS       FAXLNP           13
023500970123     C                   MOVEL     *BLANKS       TELLNP           13
023600970123     C     SIMFEL        CHAIN     AZORG                              31
023700970123    4C     *IN31         IFEQ      *OFF
023800970123     C                   MOVEL     ORGDES        DESLNP
023900970123     C                   MOVEL     ORGIND        INDLNP
024000970123     C                   Z-ADD     ORGCPF        CAPLNP
024100970123     C                   MOVEL     ORGLOC        LOCLNP
024200970123     C                   MOVEL     ORGPRO        PROLNP
024300970123     C                   MOVEL     ORGTEL        TELLNP
024400970123     C                   MOVEL     ORGFAX        FAXLNP
024500970123     C                   EXSR      CUCITO
024600970123    4C                   END
024700970123     C                   MOVEL     D82RSX        WRAG             38
024800970123     C                   MOVEL     D82CAP        CAP9              9
024900970124     C                   MOVEL     D82LOC        WCIT             30
025000970123     C                   MOVEL     D82PRO        WPRV
025100970123     C* STAMPO TOTALI
025200970528     C                   EXSR      PRTTOT
025300970528     C***
025400970123     C* SE HO STAMPATO TOTALI IN FAXIN (10 ON) LI RISTAMPO ANCHE IN
025500970123     C* CODA DI STAMPA
025600970123    4C     *IN10         IFEQ      *ON
025700970123     C                   CLOSE     MANTOT
025800041021     C***
025900970124     C                   MOVEL     CMD(1)        COMMAN
026000970123     C                   CALL      'QCMDEXC'                            90
026100970123     C                   PARM                    COMMAN           80
026200970123     C                   PARM                    LUNG             15 5
026300041021     C***
026400970123     C                   OPEN      MANTOT
026500970528     C                   EXSR      PRTTOT
026600970123    4C                   END
026700970123    3C                   END
026800970113     C*
026900010509     C* ha eseguito questa parte solo se diverso da Solo x DpD
027000010509    3C                   EndIF
027100010509     C*
027200970113     C* 20 ON --> ELABORAZIONE DEFINITIVA
027300970123    3C     *IN20         IFEQ      *ON
027400970114     C*
027500970114     C* Chiudo il f.v. e aggiorno la targa
027600970113     C                   MOVEL     'S'           FGVFCF
027700970114     C                   MOVEL     D82TRM        FGVTRM
027800970115     C* Scrivo file di appoggio da inviare in sede
027900970115     C                   CLEAR                   RGVATB
028000970115     C                   WRITE     FNFGV046
028100970113     C                   UPDATE    FNFGV000
028200970123    3C                   END
028300970124     C* Deleto records da workfile
028400970528     C                   EXSR      DELTRE
028500970123   X2C                   ELSE
028600970123     C   20              UNLOCK    FNFGV01L
028700970123    2C                   END
028800970114     C*
028900970113    1C                   ENDIF
029000960530     C*
029100000000     C                   SETON                                        LR
029200960529     C*****************************************************************
029300960529     C* OVRPRTF: Ovrprtf file di stampa
029400960529     C*****************************************************************
029500960529     C     OVRPRT        BEGSR
029600021017     C*
029700041021     C                   Exsr      Code_x_FAX
029800041021     C*
029900021017     C                   Z-ADD     65            LUNG
030000021017     C*
030100970124     C* SI DOGANA - SU ALMENO UNA DELLE LNA DEL FV:TOTALI ANCHE VIA FAX
030200970218    2C     D82TFF        IFEQ      'F'
030300041021     C**
030400041021     C**  però adesso bisogna fare Attenzione:  --> E-MAIL !!!!
030500041021     C**    se c'è l'indirizzo E-mail e prima era previsto l'invio con il FAX,
030600041021     C**    non deve essere più inviato il FAX ma tutto viene trasmesso con e-mail.
030700041021     C**  Per l'OVRPRTF tramite posta elettronica si utilizza il TRUL44R
030800041021     C**  che imposta i parametri oltre che reperisce anche l'indirizzo e-mail.
030900041021     c                   clear                   trul44ds
031000050913     c                   clear                   trtcm1ds
031100041021     c                   eval      d44pgm = 'TRTC82R'
031200041025     c                   eval      d44cl = '200'
031300041021     c                   move      D82CPT        d44ksc
031400041021     c                   eval      d44ovr = 'S'
031500041021     c                   eval      d44scope='*JOB'
031600041021     c                   eval      d44prtf = 'MANTOT'
031700120305     c***66              eval      d44obj = '*OBJM*Bartolini Total '+
031800120305     c   66              eval      d44obj = '*OBJM*Bartolini/BRT Total '+
031900041110     c                             'Manifest del ' + %editw(PARDFV:'  /  /    ')
032000041110     c                              + ' - ' + %trim(D82RSX)
032100120305     c**n66              eval      d44obj = '*OBJM*Bartolini/BRT Total '+
032200120305     c  n66              eval      d44obj = '*OBJM*BRT Total '+
032300110510     c                             'Manifest del ' + %editw(PARDFV:'  /  /    ')
032400110510     c                              + ' - ' + %trim(D82RSX)
032500041021     c                   call      'TRUL44R'
032600041021     c                   parm                    trul44ds
032700050913     c                   parm                    trtcm1ds
032800041021     C     D44err        ifeq      *BLANKS
032900041021     C                   SETON                                        10
033000041021     c                   end
033100041021     C*
033200970114     C* Ho il n.fax dogana --> invio totali via fax
033300041021     C*  e non è riuscita l'invio con E-Mail
033400041021     C     D82DGX        ifne      *BLANKS
033500041021     C     D44err        andne     *BLANKS
033600021017     C                   MOVEL     CMD(2)        CMDPRT
033700021017     C                   MOVEL     FaxInOutQ     OUTQPR
033800021017     C                   MOVEL     CMDPRT        COMMAN
033900041021     C*
034000970114     C                   CALL      'QCMDEXC'                            90
034100970114     C                   PARM                    COMMAN           80
034200970114     C                   PARM                    LUNG             15 5
034300041021     C*
034400970114     C                   SETON                                        10
034500041021     c                   end
034600041021     C*
034700041021    2C                   End
034800041021     C* Totali su carta se non è stato trasmesso nulla oppure se non doveva
034900041021     C*   essere trasmesso.
035000041021    2C     D82TFF        IFne      'F'
035100041021    2C     *in10         oreq      *off
035200970124     C                   MOVEL     CMD(1)        COMMAN
035300970114     C                   CALL      'QCMDEXC'                            90
035400970114     C                   PARM                    COMMAN           80
035500970114     C                   PARM                    LUNG             15 5
035600041021    2C                   End
035700041021     C*
035800970114     C                   OPEN      MANTOT
035900970114     C*
036000960529     C                   ENDSR
036100041021      *****************************************************************
036200041021      * Imposta le code x invio FAX
036300041021      *****************************************************************
036400041021     C     Code_x_FAX    BEGSR
036500041021      *
036600041021     ** Reperisco la coda delle stampe FAXIN .
036700041021     C                   CALL      'TRULOUTQ'
036800041021     ** Parametri di input.
036900041021     C                   PARM                    PrmFine
037000041021     C                   PARM      SimPOU        PrmFilS
037100041021     C                   PARM      FaxIn         PrmTpOutQ
037200041021     ** Parametri di output.
037300041021     C     MyOutQ        PARM      *BLANK        PrmOutQ
037400041021     C     MyOutQLib     PARM      *BLANK        PrmOutQLib
037500041021     C                   PARM      *BLANK        PrmEsito
037600041021     ** Se il reperimento della coda non è riuscito, creo lo spool in QDKT.
037700041021     C                   IF        PrmEsito = Errore
037800041021     C                   EVAL      MyOutQ = 'QDKT'
037900041021     C                   EVAL      MyOutQLib = '*LIBL'
038000041021     C                   ENDIF
038100041021      *  Se non ha trovato imposta la coda generica
038200041021     C                   IF        MyOutQ = *BLANK
038300041021     C                   EVAL      MyOutQ = 'QDKT'
038400041021     C                   EVAL      MyOutQLib = '*LIBL'
038500041021     C                   ENDIF
038600041021      *
038700041021     C                   EVAL      FaxInOutQ = MyOutQ
038800041021     C                   EVAL      FaxInLibQ = MyOutQLib
038900041021     C*
039000041021     C                   ENDSR
039100970115     C*****************************************************************
039200970115     C* CANCELLO RECORDS DA WORKFILE E SE ELABORAZIONE DEFINITIVA
039300970115     C* CANCELLO ANOMALIE 645
039400970114     C*****************************************************************
039500970528     C     DELTRE        BEGSR
039600050523     C     KTRE0         SETLL     wfman01L
039700050523     C     KTRE0         READE     wfman01L                               31
039800970115    1C     *IN31         DOWEQ     *OFF
039900970115     C* Deleto le anom. 645 con anmdao=d82dfv e per lna solo se no BIS
040000970115    2C     *IN20         IFEQ      *ON
040100970117     C     *IN22         ANDEQ     *OFF
040200970115     C                   Z-ADD     645           KCAA
040300990519     C     KANM04        SETLL     FNANM04L
040400990519     C     KANM04        READE     FNANM04L                               31
040500970115    3C     *IN31         DOWEQ     *OFF
040600990519     C                   DELETE    FNANM000
040700990519     C     KANM04        READE     FNANM04L                               31
040800970115    3C                   ENDDO
040900970115    2C                   END
041000050530     C                   DELETE    TREXP000
041100050523     C     KTRE0         READE     wfman01L                               31
041200970115    1C                   ENDDO
041300970127     C* SE DEFINITIVA RIPETO GIRO ANCHE PER RECORDS PROVVISORI
041400970127     C     *IN20         IFEQ      *ON
041500970127     C                   MOVE      'P'           WTRC
041600050523     C     KTRE0         SETLL     wfman01L
041700050523     C     KTRE0         READE     wfman01L                               31
041800970127    1C     *IN31         DOWEQ     *OFF
041900050523     C                   DELETE    TREXP000
042000050523     C     KTRE0         READE     wfman01L                               31
042100970127    1C                   ENDDO
042200970127     C                   END
042300970114     C                   ENDSR
042400970120     C***------------------------------------------------***
042500970120     C     CUCITO        BEGSR                                                  *
042600970120     C                   MOVEA     *BLANKS       DATX                           *
042700970120     C     '  '          SCAN      ORGDES        W                 2 0          *
042800970120     C     W             IFEQ      0                                            *
042900970120     C                   Z-ADD     21            W                              *
043000970120     C                   END                                                    *
043100970120     C                   MOVEA     ORGDES        DATX                           *
043200970120     C                   MOVEL     ','           DATX(W)                        *
043300970120     C                   ADD       2             W                              *
043400970120     C     2             SUBST     STRD          APPO              2            *
043500970120     C                   MOVEA     APPO          DATX(W)                        *
043600970120     C                   ADD       2             W                              *
043700970120     C                   MOVEA     '/'           DATX(W)                        *
043800970120     C                   ADD       1             W                              *
043900970120     C     2             SUBST     STRD:3        APPO                           *
044000970120     C                   MOVEA     APPO          DATX(W)                        *
044100970120     C                   ADD       2             W                              *
044200970120     C                   MOVEA     '/'           DATX(W)                        *
044300970120     C                   ADD       1             W                              *
044400970120     C     4             SUBST     STRD:5        APPO4             4            *
044500970120     C                   MOVEA     APPO4         DATX(W)                        *
044600970120     C                   MOVEA     DATX          DATAX            30
044700970120     C                   ENDSR                                                  *
044800970528     C*****************************************************************
044900970528     C* STAMPA TOTALI DEL FOGLIO VIAGGIO
045000970528     C*****************************************************************
045100970528     C     PRTTOT        BEGSR
045200970528     C* Stampo intestazione e corpo della lettera
045300970528     C                   EXCEPT    RIGTOT
045400970528     C* Ciclo per stampa totali colli e peso per lna
045500970528     C                   Z-ADD     1             II
045600970528     C     II            DOWLE     20
045700970528     C     LNA(II)       ANDGT     *ZEROS
045800970528     C                   EXCEPT    RIGTO1
045900970528     C                   ADD       1             II
046000970528     C                   ENDDO
046100970528     C* Stampo righe finali
046200970528     C                   EXCEPT    RIGFIN
046300970528     C                   ENDSR
046400960529     C*-----------------------------------------------------------------*
046500960529     C*  Operazioni iniziali
046600960529     C*-----------------------------------------------------------------*
046700960529     C     *INZSR        BEGSR
046800960529     C*
046900960529     C*  Reperisco dati da video
047000960529     C     *ENTRY        PLIST
047100960529     C                   PARM                    KPJBA
047200000405     C                   MOVEL     KPJBU         TRTC82DS
047300050530      **
047400050530      ** Abilita emissione stampa del Volume se richiesto su tab.PU
047500050530      **
047600050530     c                   setoFF                                       88
047700050530     c                   if        D82VMA ='S'
047800050530     c                   seton                                        88
047900050530     c                   end
048000050530      **
048100010509     C*
048200010509      * Controlla subito se è stato chiamato per chiudere una DPD
048300010509      *  quindi è stato chiamato direttamente dal TRTC82R
048400010509      *   e non dalla procedura chiamata con l'azione batch TC83.
048500010509      * Se chiamato direttamente con CALL ha nel parametro MEMBRO
048600010509      *  della DS TRTC82DS un codice particolare:
048700010509     C                   clear                   soloDPD           1
048800060102      *--
048900060102      * non è + in funzione questo tipo di controllo sul TRTC82R
049000060102      *--
049100010509     C     D82MBR        Ifeq      'SOLO_X_DPD'
049200010509     C                   movel     'S'           soloDPD
049300010509     C                   End
049400060102      *--  quindi soloDPD rimane sempre vuoto
049500060102      *--
049600010509      *
049700960529     C*  Richiamo XPARUT
049800960529     C                   Z-ADD     1             CODUT
049900960529     C                   CALL      'X§PARUT'
050000960529     C                   PARM                    UT§DSE
050100960529     C                   MOVEL     REC80         CNCR80
050200110511     C                   MOVEL     REC42         CNCR42
050300110511     C*******            MOVEL     RAGUT         DITTA            20
050400110511     C                   MOVEL     simRGS        DITTA            20
050500960529     C*---------------------------------------------------------------*
050600970113     C*
050700970528     C     KTRE          KLIST
050800970113     C                   KFLD                    D82NFV
050900970113     C                   KFLD                    SIMFEL
051000970113     C                   KFLD                    WTRC
051100970113     C                   KFLD                    WLNA
051200970113     C*
051300970528     C     KTRE0         KLIST
051400970113     C                   KFLD                    D82NFV
051500970113     C                   KFLD                    SIMFEL
051600970113     C                   KFLD                    WTRC
051700970123     C*
051800970123     C     KANM04        KLIST
051900970123     C                   KFLD                    KCAA
052000970123     C                   KFLD                    DFV06
052100970528     C                   KFLD                    TRELNA
052200020808     c*
052300020808     c     K02FGV01      klist
052400020808     c                   kfld                    D82nfv
052500020808     c                   kfld                    SIMfel
052600020808     c*
052700020808     c     K02FGW01      klist
052800020808     c                   kfld                    FGVnfv
052900020808     c                   kfld                    FGVlnp
053000961216     C*
053100110510     C     ktbe          klist
053200110510     C                   kfld                    tbecod
053300110510     C                   kfld                    tbeke1
053400110510     C                   kfld                    tbeke2
053500020808     ***C*---------------------------------------------------------------*
053600020808     ***C*  Definizione variabili
053700020808     ***C*
053800020808     ***C     *LIKE         DEFINE    D82DFV        PARDFV
053900020808     ***C*
054000020808     ***C*
054100020808     ***C     *LIKE         DEFINE    D82LNP        WLNA
054200020808     ***C     *LIKE         DEFINE    TRETRC        WTRC
054300020808     ***C     *LIKE         DEFINE    TRETOP        TOGPES
054400020808     ***C     *LIKE         DEFINE    TRETOC        TOGNCL
054500020808     ***C     *LIKE         DEFINE    ANMCAA        KCAA
054600020808     ***C     *LIKE         DEFINE    ANMDAO        DFV06
054700020808     ***C     *LIKE         DEFINE    D82PRO        WPRV
054800960529     C*---------------------------------------------------------------*
054900960529     C                   TIME                    UTIME             6 0
055000960529     C* PRENDO LA DATA DEL GIORNO DA TIME
055100960529     C                   TIME                    W0140            14 0
055200970120     C                   MOVE      W0140         STRD              8
055300060109     C                   MOVE      W0140         Wdatagg
055400060109     C     *eur          MOVEL     Wdatagg       Wdataoggi
055500060109     C     *iso          MOVEL     Wdataoggi     Wdatagg
055600110510      *
055700110510     c                   move      'VPO'         tbecod
055800110510     c                   movel     'BRT'         tbeke1
055900110510     c                   clear                   tbeke2
056000110510     c     ktbe          chain     tntbe01l
056100110510     c                   if        %found(tntbe01l)
056200110510     c                   movel     tbeuni        dvpobrt
056300110510     c                   if        Wdatagg <= §VPOBRTDTA
056400110510     c                   seton                                        66
056500110510     c                   endif
056600110510     c                   endif
056700120305      *
056800120305      ***cambio di marchio in BRT finale x il 15/3
056900120305     c                   setoff                                       66
057000120305     c                   if        Wdatagg <  20120315
057100120305     c                   seton                                        66
057200120305     c                   endif
057300110510     C*
057400970124     C*
057500970124     C* Operazione fittizia per giustificare il file in input
057600970124     C                   READ      FNFGV046                               31
057700961114     C*
057800961114     C     D82DEF        COMP      'SI'                                   20
057900970123     C     D82FVB        COMP      'B'                                    22
058000970123     C                   MOVE      D82DFV        DFV06
058100970124     C*  IMPOSTO DATA FOGLIO VIAGGIO PER STAMPA
058200970124     C                   Z-ADD     D82DFV        DATA
058300970124     C                   Z-ADD     MM            GGMM              4 0
058400970124     C                   MOVEL     GG            GGMM
058500970124     C                   Z-ADD     AA            PARDFV
058600970124     C                   MOVEL     GGMM          PARDFV
058700970114     C*
058800970114     C*  Data CMR
058900970114     C                   Z-ADD     D82DCM        DATA
059000970114     C                   Z-ADD     MM            GGMM              4 0
059100970114     C                   MOVEL     GG            GGMM
059200970114     C                   Z-ADD     AA            DATCMR            8 0
059300970114     C                   MOVEL     GGMM          DATCMR
059400970114     C*
059500970113     C* IMPOSTO TIPO RECORD PER LETTURE WORK FILE
059600970113     C  N20              MOVE      'P'           WTRC
059700970113     C   20              MOVE      'D'           WTRC
059800960529     C*
059900960529     C                   ENDSR
060000970114     O*****************************************************************
060100970114     O* TOTALI FOGLIO VIAGGIO VIA FAX
060200970114     O*****************************************************************
060300970120     OMANTOT    E            RIGTOT           05
060400970120     O                                           46 '*'
060500970120     O                                           50 'FAX:'
060600970120     O                       D82DGX              67
060700970120     O          E            RIGTOT           06
060800970120     O                                           75 'Pag.:'
060900970120     O                                           80 '1'
061000970120     O          E            RIGTOT           08
061100970120     O                                         +  6 'Punto Operativo di'
061200970120     O          E            RIGTOT           09
061300970120     O                       DESLNP            +  6
061400970120     O          E            RIGTOT           10
061500970120     O                       INDLNP            +  6
061600970120     O          E            RIGTOT           11
061700970120     O                       CAPLNP            +  6
061800970120     O                       LOCLNP            +  1
061900970120     O                       PROLNP            +  1
062000970120     O                                           53 'Spettabile'
062100970120     O          E            RIGTOT           13
062200970120     O                                         +  6 'FAX'
062300970120     O                       FAXLNP            +  1
062400970120     O                       WRAG                81
062500970120     O          E            RIGTOT           14
062600970120     O                                         +  6 'TEL'
062700970120     O                       TELLNP            +  1
062800970120     O                       CAP9                52
062900970120     O                       WCIT              +  1
063000970120     O                                           81 '       '
063100970120     O                       WPRV                77
063200970120     O          E            RIGTOT           16
063300970120     O                       DATAX             +  6
063400970120     O          E            RIGTOT           19
063500970120     O                       TOT(1)              66
063600970120     O          E            RIGTOT           20
063700970120     O                       TOT(2)              66
063800970120     O                       PARDFV              36 '  /  /    '
063900970120     O          E            RIGTOT           22
064000970120     O                       TOT(3)              66
064100970528     O                       DATCMR              40 '  /  /    '
064200970120     O          E            RIGTOT           24
064300970120     O                       TOT(4)              66
064400970528     O                       D82CMR              65
064500970120     O          E            RIGTOT           26
064600970120     O                       TOT(5)              66
064700970528     O                       D82TRM              40
064800050525      **
064900970120     O          E            RIGTOT           28
065000970120     O                       TOT(6)              66
065100050530     o               88                          79 '--------------+'
065200050530      *
065300970528     O          E            RIGTOT           29
065400970120     O                       TOT(7)              66
065500050530     o               88                          79 'TOT.VOLUME MC |'
065600050530      *
065700970528     O          E            RIGTOT         1 30
065800970528     O                       TOT(8)              66
065900050530     o               88                          79 '--------------+'
066000050530      *
066100050525      **
066200970528     O          E            RIGTO1         1
066300970528     O                       LNA(II)             18
066400970528     O                                           19 '-'
066500970528     O                       DES(II)             29
066600970528     O                       TOC(II)       Z     40
066700970528     O                       PES(II)       4     58
066800050530     o               88      tov(II)       4     77
066900050525      **
067000970528     O          E            RIGFIN         1
067100970528     O                       TOT(10)             66
067200050530     o               88                          79 '--------------+'
067300050530      **
067400970528     O          E            RIGFIN         1
067500970528     O                                           26 'T O T A L E'
067600970528     O                       TOGNCL        Z     40
067700970528     O                       TOGPES        4     58
067800050530     o               88      togvlm        4     77
067900050525      **
068000970528     O          E            RIGFIN         2
068100970528     O                       TOT(11)             66
068200050530     o               88                          79 '--------------+'
068300050530      **
068400970528     O          E            RIGFIN      3  2
068500970128     O                                           22 'Distinti Saluti.'
068600970528     O          E            RIGFIN         1
068700980309     O                       DITTA               66
068800970528** CMD
068900970123OVRPRTF MANTOT QSYSPRT SAVE(*YES)                                    1
069000021017OVRPRTF MANTOT QSYSPRT OUTQ(FAXIN     ) SAVE(*YES) OUTPTY(4)         2
069100970114**
069200970120      Con la presente vi comunichiamo i seguenti dati relativi al-   1
069300970120      le merci spedite il xx/xx/xxxx:                                2
069400970528             DATA   CMR     : XX/XX/XXXX                             3
069500970528             NUMERO CMR     : 12345678901234567890123456789012345    4
069600970528             TARGA AUTOMEZZO: 1234567890                             5
069700970528             +----------------+-----------+-------------------+      6
069800970528             | LINEA DI ARRIVO| TOT.COLLI | TOT.PESO LORDO KG |      7
069900970528             +----------------+-----------+-------------------+      8
070000970528               XXX-XXXXXXXXXX    1234567       123456789,0           9
070100050525             -----------------+-----------+-------------------+     10
070200970528             --------------------------------------------------     11
