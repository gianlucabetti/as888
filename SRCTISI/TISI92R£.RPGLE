000100000000     H*------------------------------------------------------------------------*
000200020514     H* SIMULAZIONE DELIVERY E TASSAZIONE - RICHIESTA PARAMETRI                *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800970707     FTISI92D   CF   E             WORKSTN INFDS(DEVDS1)
000900970707     F                                     SFILE(SI92DVM:MSGULT)
001000000000     FAZORG01L  IF   E           K DISK
001100000000     FTABEL00F  IF   E           K DISK
001200960617     FCNACO00F  IF   E           K DISK
001300960617     FTNTAM01L  IF   E           K DISK
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*-------------------
001800000000     D* SCHIERE A TEMPO DI COMPILAZIONE
001900000000     D*-------------------
002000120509     D MSG             S             78    DIM(48) CTDATA PERRCD(1)             *MESSAGGI
002100000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002200000000     D*-------------------
002300000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002400000000     D*-------------------
002500000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002600000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002700000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002800000000     D*-------------------
002900000000     D* TABELLA DESCRIZIONI A VIDEO
003000000000     D*-------------------
003100000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003200000000     D*-------------------
003300000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003400000000     D*-------------------
003500000000     D KEYS            S              1    DIM(40)
003600960617     D*-------------------
003700960617     D* CAPOCONTO / SOTTOCONTO DI RITORNO "XCLIR"
003800960617     D*-------------------
003900960617     D KSC             S              7  0 DIM(30)
004000960617     D KSA             S              4    DIM(30)                              *ALFANUMERICO
004100000000     D*------------------------------------------------------------------------*
004200000000     D* INPUT
004300000000     D*------------------------------------------------------------------------*
004400000000     D*-------------------
004500000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004600000000     D*-------------------
004700000000     D DSFUN         E DS
004800000000     D  FUNRIG                 1     40
004900000000     D*-------------------
005000000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005100000000     D*-------------------
005200000000     D DSMSG         E DS
005300000000     D*-------------------
005400000000     D* DS PER GESTIONE TASTI FUNZIONALI
005500000000     D*-------------------
005600000000     D DSKEYS        E DS
005700000000     D  KEYRIG                 1     40
005800000000     D*-------------------
005900000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006000000000     D*-------------------
006100000000     D                SDS
006200000000     D  USER                 254    263
006300000000     D*-------------------
006400000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006500000000     D*-------------------
006600000000     D DEVDS1          DS
006700000000     D  KEY                  369    369
006800000000     D  CURSOR               370    371
006900020225     D*-------------------
007000020225     D* DS DEFINIZIONE TABELLA "NETWORK"
007100020225     D*-------------------
007200020225     D DNTW          E DS
007300020225     D*-------------------
007400020225     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
007500020225     D*-------------------
007600160302     D TRULC7DS      E DS
007700110922     D*-------------------
007800110922     D* DS PER TRUL73R - CONTROLLO LIMITI per tipo servizio
007900110922     D*-------------------
008000110922     D trul73ds      E DS
008100000000     D*------------------
008200000000     D* DS CONTROLLO DATE (8)
008300000000     D*------------------
008400000000     D WLBDA8          DS                  INZ
008500000000     D  G08DAT                 1      8  0
008600000000     D  G08INV                 9     16  0
008700000000     D  G08ERR                17     17
008800000000     D  G08TGI                18     22  0
008900000000     D*------------------
009000000000     D* DS DATA (8)
009100000000     D*------------------
009200000000     D                 DS
009300000000     D  GIOR                   1      2  0
009400000000     D  MESE                   3      4  0
009500000000     D  ANNO                   5      8  0
009600981214     D  DATA                   1      8  0
009700090506     d
009800090506     D DSta01        E DS
009900120509     D og143         E DS
010000120509
010100120509     D ds3idp        E DS
010200000000     D*------------------
010300000000     D* DS NAZIONI
010400000000     D*------------------
010500000000     D DS15          E DS
010600050413     D*------------------
010700050413     D* DS consegne particolari
010800050413     D*------------------
010900050413     D DS1P          E DS
011000000000     D*------------------
011100000000     D* DS TIPI SERVIZIO
011200000000     D DS5E          E DS
011300110623     D*------------------
011400110623     D* DS TIPI inoltro
011500110623     D DS9A          E DS
011600970704     D*-------------------
011700970704     D* DS VALUTE
011800970704     D*-------------------
011900970704     D DSCV          E DS
012000010823     D DSCW          E DS
012100970704     D*-------------------
012200970704     D* DS TIPI INCASSO C/ASSEGNO
012300970704     D*-------------------
012400970704     D DS1A          E DS
012500970704     D*------------------
012600970704     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
012700970704     D*------------------
012800970704     D DSXC          E DS                  EXTNAME(XCLIRDS)
012900970704     D*---
013000970704     D                 DS
013100970704     D  DSKSN                  1      4P 0
013200970704     D  DSKSA                  1      4
013300970704     D*------------------
013400970704     D* DS "TISI00R" - INTERROGAZIONE ARCHIVI CAPPARIO
013500970704     D*------------------
013600970704     D DSSI00        E DS                  EXTNAME(TISI00DS)
013700000000     D*------------------
013800970704     D* DS "TISI95R" CONTROLLA CAP
013900000000     D*------------------
014000970704     D DSSI95        E DS                  EXTNAME(TISI95DS)
014100090615     D fnlv13ds      E DS
014200090615     d
014300090615     D tisi97ds      E DS
014400090616     D dcppflo       E DS
014500970704     D*-------------------
014600970704     D* DS DI PROCEDURA
014700970704     D*-------------------
014800970707     D DSSI92        E DS                  EXTNAME(TISI92DS) INZ
014900970704     D*-------------------
015000970704     D* ARCHITETTURA
015100970704     D*-------------------
015200970704     D KPJBA         E DS
015300020225     D*-------------------
015400020225     D* VARIABILI DI WRK
015500020225     D*-------------------
015600020225     D DATA_EUR        S               D   DATFMT(*eur)
015700020925     D*------------
015800020925     D* DS REPERIMENTO DATI UTENTE
015900020925     D*------------
016000020925     D TIBS34DS      E DS                                                       *Profili utente
016100020925     D DDATIUTE      E DS                                                       *Dati utente
016200020925     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
016300100406     d* Controllo abilitazioni utente
016400100406     d tntaa1ds      e ds
016500020925     D*------------
016600020925     D* DS CONTROLLO/SCELTA TABELLE
016700020925     D*------------
016800020925     D TIBS02DS      E DS                                                       *Ricerca tabella
016900090429     D*-------------------
017000090429     D* PASSO IN SCELTA COD TARIFFA DI TARIFFA/OFFERTA - TNTA36R -
017100090429     D PARAM4          DS
017200090429     D  PA4COD                 6     12  0
017300090429     D  PA4CTR                13     15
017400090429     D  PA4FLG                16     16
017500090429     D  PA4VPR                17     17
017600090429     D  PA4CCC                18     21  0
017700090429     D  PA4DCV                22     36
017800090429     d  pa4prg                37     39
017900090429     d  pa4dtr                40     47
018000020925     D*------------------
018100020925     D* SCHIERA X LINEEE CONSENTITE
018200020925     D*------------------
018300020925     D L1              S              3    DIM(250)
018400030320
018500030403     d wflg            s              1    Inz
018600090429     d forzaPA         s              1    Inz
018700090429     d forzaBLO        s              1    Inz
018800090506     d forzaIE         s              1    Inz
018900090616     d forzaCapUti     s              1    Inz
019000040301     d p26vol          s              8  3
019100050413     d wtcx            s                   like(v1ctc1)
019200050413     d wdtcx           s                   like(v1dtc1)
019300090618     d svtbluni        s                   like(tbluni)
019400090616     d savksc          s                   like(v1cksc) inz
019500090616     d savctr          s                   like(v1cctr) inz
019600090616     d savtpo          s                   like(v1ctpo) inz
019700090616     d savntw          s                   like(v1cntw) inz
019800090616     d savcas          s                   like(v1ccas) inz
019900090618     d savcad          s                   like(v1ccad) inz
020000090618     d savnar          s                   like(v1cnar) inz
020100110923     d wncl_pre        s                   like(v1cncl) inz
020200110923     d wpes_pre        s                   like(v1cpkb) inz
020300110923     d wvol_pre        s                   like(v1cvlf) inz
020400110923     d savtsp          s                   like(v1ctsp) inz
020500120608     d savpkb          s                   like(v1cpkb) inz
020600160302     d savt_ntw        s                   like(ic7tntw) inz
020700030320
020800000000     C*------------------------------------------------------------------------*
020900000000     C* MAIN LINE
021000000000     C*------------------------------------------------------------------------*
021100000000     C*
021200000000     C* CICLO FINO A CHE:
021300000000DO  1C     $FINE         DOUEQ     '1'
021400000000     C*
021500000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
021600000000     C*
021700000000     C* CONTROLLA GESTIONE VIDEO
021800000000IF  2C     $GES          IFEQ      'V1'
021900000000     C                   EXSR      GESV1                                        *PARAMETRI
022000000000E   2C                   ENDIF
022100000000     C*
022200000000E   1C                   ENDDO
022300000000     C* FINE PGM
022400960729     C                   EXSR      CHIUDI                                       *CHIUDE FILE
022500000000     C                   SETON                                        LR
022600000000     C*------------------------------------------------------------------------*
022700000000     C* CHIUDI  - CHIUDE EVENTUALI PGM CON USCITA RETURN
022800000000     C*------------------------------------------------------------------------*
022900000000     C     CHIUDI        BEGSR
023000970704     C* TISI95R
023100970704     C                   CLEAR                   DSSI95
023200970704     C                   MOVEL     'C'           I95TLA                         'C'=CHIUDI
023300970704     C                   CALL      'TISI95R'                            99
023400970704     C                   PARM                    DSSI95
023500100406     C*
023600100406     C                   clear                   tntaa1ds
023700100406     C                   movel     'C'           Itaa1Tla
023800100406     C                   movel(p)  tntaa1ds      kpjbu
023900100406     C                   CALL      'TNTAA1C'
024000100406     C                   PARM                    KPJBA
024100110922     c*
024200110922     C                   clear                   trul73ds
024300110922     C                   movel     'C'           I73tla
024400110922     C                   CALL      'TRUL73R'
024500110922     C                   PARM                    trul73ds
024600100406     c
024700000000     C*
024800000000     C                   ENDSR
024900000000     C*------------------------------------------------------------------------*
025000000000     C* GESV1 - GESTIONE VIDEO
025100000000     C*------------------------------------------------------------------------*
025200000000     C     GESV1         BEGSR
025300000000     C*
025400000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
025500000000     C                   EXSR      CARV1                                        *CARICA VIDEO
025600000000E   1C                   ENDIF
025700000000     C*
025800000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
025900000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
026000000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
026100000000E   1C                   ENDIF
026200000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
026300000000     C*
026400970707     C                   EXFMT     SI92DV1                                      *PARAMETRI
026500000000     C*
026600000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
026700000000     C*
026800000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
026900000000SL  2C                   SELECT
027000000000     C     KEY           WHENEQ    ENTER
027100000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
027200000000     C     KEY           WHENEQ    F6
027300000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
027400030320     C     KEY           WHENEQ    F11
027500040301     C                   EXSR      F11GES                                       *GEST.F11(Volume)
027600000000     C                   OTHER
027700000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
027800000000E   2C                   ENDSL
027900000000E   1C                   ENDIF
028000000000     C*
028100000000     C                   ENDSR
028200000000     C*------------------------------------------------------------------------*
028300000000     C* CARV1 - CARICA VIDEO
028400000000     C*------------------------------------------------------------------------*
028500000000     C     CARV1         BEGSR
028600000000     C*
028700000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
028800000000     C*
028900000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
029000000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
029100000000     C*
029200970707     C                   WRITE     SI92DVT                                      *TESTATA
029300970707     C                   WRITE     SI92DVF                                      *FUNZIONI
029400000000     C*
029500000000     C                   ENDSR
029600000000     C*------------------------------------------------------------------------*
029700000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO
029800000000     C*------------------------------------------------------------------------*
029900000000     C     ENTV1         BEGSR
030000000000     C*
030100000000     C* CONTROLLA I CAMPI IMMESSI
030200000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
030300000000     C*
030400000000     C                   ENDSR
030500000000     C*------------------------------------------------------------------------*
030600000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
030700000000     C*------------------------------------------------------------------------*
030800000000     C     F06GES        BEGSR
030900000000     C*
031000000000     C* CONTROLLA I CAMPI IMMESSI
031100000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
031200000000     C*
031300000000     C* NON CI SONO ERRORI
031400000000IF  1C     $ERR          IFEQ      '0'
031500000000     C*
031600000000     C* LANCIA PROGRAMMA
031700001115     C                   CLEAR                   DSSI92
031800000000     C                   MOVEL     '0'           D92F03
031900000000     C                   MOVEL     '0'           D92F12
032000000000     C                   MOVEL     '0'           D92ERR
032100000000     C                   MOVEL     *BLANKS       D92MSG
032200000000     C                   Z-ADD     DSPV1C        D92DSP
032300000000     C                   MOVEL     V1CLNP        D92LNP
032400000000     C                   MOVEL     V1CNAR        D92NAR
032500000000     C                   MOVEL     V1CCAD        D92CAD
032600970704     C                   MOVEL     V1CLOD        D92LOD
032700000000     C                   MOVEL     V1CTSP        D92TSP
032800000000     C                   MOVEL     V1CISO        D92ISO
032900980116     C                   MOVEL     V1CFFD        D92FFD
033000000919     C                   MOVEL     V1CFDN        D92FDN
033100980703     C                   MOVEL     V1CTPO        D92TPO
033200980703     C                   MOVEL     V1CNZM        D92NZM
033300980703     C                   MOVEL     V1CCAM        D92CAM
033400980703     C                   MOVEL     V1CLOM        D92LOM
033500960528     C                   Z-ADD     V1CNCL        D92NCL
033600960528     C                   Z-ADD     V1CPKB        D92PKB
033700960528     C                   Z-ADD     V1CVLF        D92VLF
033800960617     C                   MOVEL     V1CKSC        D92KSC
033900960614     C                   MOVEL     V1CCTR        D92CTR
034000010822     C                   Z-ADD     V1CCAS        D92CAS
034100960926     C                   MOVEL     V1CVCA        D92VCA
034200960926     C                   MOVEL     V1CTIC        D92TIC
034300010822     C                   Z-ADD     V1CIAS        D92IAS
034400960926     C                   MOVEL     V1CVAS        D92VAS
034500050415     C                   Z-ADD     V1Cqft        D92qft
034600020813     C                   Z-ADD     V1CSCO        D92SCO
034700021106     C                   MOVEL     V1CDDT        D92DDT
034800020225     C                   MOVEL     V1CNTW        D92NTW
034900050413     c                   if        v1ctc1=' ' and v1ctc2<>' '
035000050413     c                   movel     v1ctc2        d92tc1
035100050413     c                   else
035200050413     C                   MOVEL     V1Ctc1        D92tc1
035300050413     C                   MOVEL     V1Ctc2        D92tc2
035400050413     c                   endif
035500110818     c                   movel     v1cemd        d92emd
035600050413     c
035700020514     C                   MOVEL     DSSI92        KPJBU
035800970707     C                   CALL      'TISI93R'
035900000000     C                   PARM                    KPJBA
036000970707     C                   MOVEL     KPJBU         DSSI92
036100970704     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
036200000000     C*
036300000000     C* RITORNA MESSAGGIO DI ERRORE, LO VISUALIZZA
036400000000IF  2C     D92ERR        IFNE      '0'                                          *ERRORE
036500000000     C                   MOVEL     D92ERR        $ERR
036600000000     C                   MOVEL     D92MSG        $MSG
036700000000     C                   EXSR      CARMSG
036800000000X   2C                   ELSE                                                   *NO ERRORE
036900000000     C*
037000000000     C* SE RITORNA PREMUTO F3 ESCE DAL PROGRAMMA, SE F12 NO
037100000000IF  3C     D92F03        IFEQ      '1'
037200000000     C     D92F12        OREQ      '1'
037300000000     C                   MOVEL     '1'           $ERR                           *NON VA AVANTI
037400000000IF  4C     D92F03        IFEQ      '1'
037500000000     C                   MOVEL     '1'           $F03                           *FINE PGM
037600000000     C                   MOVEL     '1'           $FINE                          *FINE CICLO
037700000000E   4C                   ENDIF
037800000000E   3C                   ENDIF
037900000000E   2C                   ENDIF
038000000000     C*
038100000000E   1C                   ENDIF
038200030403
038300030403     c                   Clear                   wflg
038400000000     C*
038500000000     C                   ENDSR
038600030320     C*------------------------------------------------------------------------*
038700030320     C* F11GES - GEST. TASTO FUNZ. F11 ==> Calcolo volume
038800030320     C*------------------------------------------------------------------------*
038900030320     C     F11GES        BEGSR
039000030320
039100090429     c                   If        wflg = *Blanks
039200030403     c                   Clear                   v1cvlf
039300030403     c                   Eval      wflg = '1'
039400030403     c                   EndIf
039500040301
039600040301     c                   Clear                   p26vol
039700040301     c                   Call      'TRUL36R'
039800040301     c                   Parm                    p26vol
039900030403
040000040301     C                   Add       p26vol        v1cvlf
040100040301     c                   Eval      $tesvd = '1'                                 *RIEMETTERE TESTATA
040200040301
040300030320     C*
040400030320     C                   ENDSR
040500000000     C*------------------------------------------------------------------------*
040600000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
040700000000     C*------------------------------------------------------------------------*
040800000000     C     DECODI        BEGSR
040900000000     C*
041000960528     C                   SETOFF                                       606162          *DELIVERY
041100000000     C                   SETOFF                                       636465
041200980116     C                   SETOFF                                       667778
041300960528     C                   SETOFF                                       676869          *TASSAZIONE
041400960925     C                   SETOFF                                       707172
041500960925     C                   SETOFF                                       737475
041600980703     C                   SETOFF                                       767980
041700020225     C                   SETOFF                                       818283
041800050413     C                   SETOFF                                       848586
041900110817     C                   SETOFF                                       8788
042000960528     C***
042100960528     C* SIMULAZIONE DELIVERY
042200960528     C***
042300000000     C*--------------------
042400000000     C* DATA SPEDIZIONE
042500000000     C*--------------------
042600000000     C     *LIKE         DEFINE    V1CDSP        DSPV1C
042700000000IF  1C     $ERR          IFEQ      '0'
042800000000     C                   Z-ADD     V1CDSP        WDA8
042900000000     C                   Z-ADD     *ZEROS        DA8W
043000000000     C                   EXSR      CHKDA8
043100000000     C                   Z-ADD     DA8W          DSPV1C
043200000000     C                   Z-ADD     WDA8          V1CDSP
043300000000IF  2C     $ERR          IFEQ      '1'
043400000000     C                   MOVEL     MSG(7)        $MSG
043500000000     C                   EXSR      CARMSG
043600000000     C                   SETON                                        60
043700000000E   2C                   ENDIF
043800000000E   1C                   ENDIF
043900000000     C*--------------------
044000000000     C* LINEA DI PARTENZA
044100000000     C*--------------------
044200000000IF  1C     $ERR          IFEQ      '0'
044300000000     C                   MOVEL     V1CLNP        WLNP
044400970704     C                   MOVEL     *BLANKS       WDLNP
044500000000     C                   EXSR      CHKLNP
044600000000     C                   MOVEL     WLNP          V1CLNP
044700000000     C                   MOVEL     WDLNP         V1DLNP
044800960704IF  2C     $ERR          IFEQ      '1'
044900000000     C                   MOVEL     MSG(8)        $MSG
045000000000     C                   EXSR      CARMSG
045100000000     C                   SETON                                        61
045200000000E   2C                   ENDIF
045300000000E   1C                   ENDIF
045400000000     C*--------------------
045500000000     C* NAZIONE
045600000000     C*--------------------
045700000000IF  1C     $ERR          IFEQ      '0'
045800970704     C                   MOVEL     V1CNAR        WNAR
045900970704     C                   MOVEL     *BLANKS       WDNAR
046000000000     C                   EXSR      CHKNAR
046100000000     C                   MOVEL     WNAR          V1CNAR
046200000000     C                   MOVEL     WDNAR         V1DNAR
046300000000IF  2C     $ERR          IFEQ      '1'
046400000000     C                   MOVEL     MSG(10)       $MSG
046500000000     C                   EXSR      CARMSG
046600080912     c                   clear                   svtbluni
046700000000     C                   SETON                                        62
046800080912     c                   else
046900080912      * mi salvo i dati della nazione che mi servono dopo
047000080912     c                   eval      svtbluni = tbluni
047100000000E   2C                   ENDIF
047200000000E   1C                   ENDIF
047300000000     C*--------------------
047400970704     C* CAP
047500000000     C*--------------------
047600000000IF  1C     $ERR          IFEQ      '0'
047700970714IF  2C     V1CCAD        IFGT      *BLANKS
047800090615     c                   clear                   tisi97ds
047900970704     C                   MOVEL     V1CNAR        WNAR
048000970704     C                   MOVEL     V1CCAD        WCAP
048100090615     c
048200970704     C                   EXSR      CHKCAP
048300970704     C                   MOVEL     WNAR          V1CNAR
048400970704     C                   MOVEL     WCAP          V1CCAD
048500970714IF  3C     $ERR          IFEQ      '1'
048600000000     C                   MOVEL     MSG(11)       $MSG
048700000000     C                   EXSR      CARMSG
048800970704     C                   SETON                                        6263
048900970714E   3C                   ENDIF
049000970714E   2C                   ENDIF
049100000000E   1C                   ENDIF
049200090618IF  4C                   IF        $err='0'   and (v1ccad<>savcad
049300090618     c                                        or   v1cnar<>savnar)
049400090618     c                   clear                   forzacaputi
049500090618     c                   eval      savcad=v1ccad
049600090618     c                   eval      savnar=v1cnar
049700090618    4c                   endif
049800970704     C*--------------------
049900970704     C* LOCALITA'
050000970704     C*--------------------
050100970704IF  1C     $ERR          IFEQ      '0'
050200970714IF  2C     V1CLOD        IFGT      *BLANKS
050300970704     C                   MOVEL     V1CNAR        WNAR
050400970704     C                   MOVEL     V1CCAD        WCAP
050500970704     C                   MOVEL     V1CLOD        WLOC
050600970704     C                   EXSR      CHKLOC
050700970704     C                   MOVEL     WNAR          V1CNAR
050800970704     C                   MOVEL     WCAP          V1CCAD
050900970704     C                   MOVEL     WLOC          V1CLOD
051000970714E   2C                   ENDIF
051100970704E   1C                   ENDIF
051200970704     C*--------------------
051300970714     C* CAP + LOCALITA'
051400970704     C*--------------------
051500970714IF  1C     $ERR          IFEQ      '0'
051600970714IF  2C     V1CCAD        IFEQ      *BLANKS
051700970714     C                   MOVEL     '1'           $ERR
051800970714     C                   MOVEL     MSG(11)       $MSG
051900970714     C                   EXSR      CARMSG
052000970714     C                   SETON                                        6263
052100970714E   2C                   ENDIF
052200970714E   1C                   ENDIF
052300970704IF  1C     $ERR          IFEQ      '0'
052400090615     c                   clear                   tisi97ds
052500970704     C                   MOVEL     V1CNAR        WNAR
052600970704     C                   MOVEL     V1CCAD        WCAP
052700970704     C                   MOVEL     V1CLOD        WLOC
052800970704     C                   EXSR      CHKCLP
052900970704IF  2C     $ERR          IFEQ      '1'
053000970704     C                   MOVEL     MSG(14)       $MSG
053100970704     C                   EXSR      CARMSG
053200980116     C                   SETON                                        64
053300970704E   2C                   ENDIF
053400970704E   1C                   ENDIF
053500000000     C*--------------------
053600000000     C* TIPO SERVIZIO
053700000000     C*--------------------
053800000000IF  1C     $ERR          IFEQ      '0'
053900000000     C                   MOVEL     V1CTSP        WTSP
054000970704     C                   MOVEL     *BLANKS       WDTSP
054100000000     C                   EXSR      CHKTSP
054200000000     C                   MOVEL     WTSP          V1CTSP
054300000000     C                   MOVEL     WDTSP         V1DTSP
054400000000IF  2C     $ERR          IFEQ      '1'
054500000000     C                   MOVEL     MSG(9)        $MSG
054600000000     C                   EXSR      CARMSG
054700000000     C                   SETON                                        65
054800000000E   2C                   ENDIF
054900000000E   1C                   ENDIF
055000000000     C*--------------------
055100110623     C* FLAG inoltro
055200000000     C*--------------------
055300000000IF  1C     $ERR          IFEQ      '0'
055400110623     c     v1ciso        andne     *blanks
055500110623     C                   MOVEL     *BLANKS       V1DISO
055600110623     C                   MOVEL     V1cISO        WINL
055700110623     C                   MOVEL     *BLANKS       WDINL
055800110623     C                   EXSR      CHKINL
055900110623     C                   MOVEL     WINL          V1cISO
056000110623     C                   MOVEL     WDINL         V1DISO
056100110623IF  2C     $err          IFEQ      '1'
056200110623     C                   SETON                                        66
056300110623     C                   MOVEL     MSG(12)       $MSG
056400110623     C                   EXSR      CARMSG
056500110623X   2C                   ELSE
056600110623E   2C                   ENDIF
056700110623E   1C                   ENDIF
056800980116     C*--------------------
056900980116     C* FERMO DEPOSITO
057000980116     C*--------------------
057100980116IF  1C     $ERR          IFEQ      '0'
057200980116IF  2C     V1CFFD        IFEQ      'S'
057300980116     C     V1CFFD        OREQ      'N'
057400980116X   2C                   ELSE
057500980116     C                   MOVEL     '1'           $ERR
057600980116     C                   MOVEL     MSG(15)       $MSG
057700980116     C                   EXSR      CARMSG
057800980116     C                   SETON                                        78
057900980116E   2C                   ENDIF
058000980116E   1C                   ENDIF
058100050428     c*
058200050428     c* Non posso forzare inoltro I o P se fermo deposito='S'
058300050428IF  1C     $ERR          IFEQ      '0'
058400050428     c                   if        v1cffd='S' and v1ciso<>' ' and v1ciso<>'C'
058500050428     C                   MOVEL     '1'           $ERR
058600050428     C                   MOVEL     MSG(36)       $MSG
058700050428     C                   EXSR      CARMSG
058800050428     C                   SETON                                        78
058900050428E   2C                   ENDIF
059000050428E   1C                   ENDIF
059100000919     C*--------------------
059200000919     C* MERCE A MAGAZZINO
059300000919     C*--------------------
059400000919IF  1C     $ERR          IFEQ      '0'
059500000919IF  2C     V1CFDN        IFEQ      'S'
059600000919     C     V1CFDN        OREQ      'N'
059700000919X   2C                   ELSE
059800000919     C                   MOVEL     '1'           $ERR
059900000919     C                   MOVEL     MSG(15)       $MSG
060000000919     C                   EXSR      CARMSG
060100000919     C                   SETON                                        82
060200000919E   2C                   ENDIF
060300000919E   1C                   ENDIF
060400980703     C*--------------------
060500980703     C* TIPO PORTO
060600980703     C*--------------------
060700980703IF  1C     $ERR          IFEQ      '0'
060800980703IF  2C     V1CTPO        IFEQ      'F'
060900980703     C     V1CTPO        OREQ      'A'
061000980703X   2C                   ELSE
061100980703     C                   MOVEL     '1'           $ERR
061200980703     C                   MOVEL     MSG(13)       $MSG
061300980703     C                   EXSR      CARMSG
061400980703     C                   SETON                                        77
061500980703E   2C                   ENDIF
061600980703E   1C                   ENDIF
061700090616
061800090616    1c                   if        $err='0' and v1ctpo<>savtpo
061900090616     c                   movel     v1ctpo        savtpo
062000090616     c                   clear                   forzaPA
062100090616     c                   clear                   forzaCAPuti
062200090616    1c                   endif
062300090617     c
062400090617IF  4C                   IF        $err='0'   and v1ccas<>savcas
062500090617     c                   clear                   forzacaputi
062600090617     c                   eval      savcas=v1ccas
062700090617    4c                   endif
062800980703     C***
062900980703     C* DATI MITTENTE OBBLIGATORI, SE PORTO ASSEGNATO
063000980703     C***
063100980703IF  1C     $ERR          IFEQ      '0'
063200980703IF  2C     V1CTPO        IFEQ      'F'                                          *FRANCO
063300980703IF  3C     V1CNZM        IFNE      *BLANKS
063400980703     C     V1CCAM        ORNE      *BLANKS
063500980703     C     V1CLOM        ORNE      *BLANKS
063600980703     C                   MOVEL     '1'           $ERR
063700980703     C                   MOVEL     MSG(28)       $MSG
063800980703     C                   EXSR      CARMSG
063900980703     C                   SETON                                        798081
064000980703E   3C                   ENDIF
064100980703X   2C                   ELSE                                                   *ASSEGNATO
064200980703     C*--------------------
064300980703     C* NAZIONE MITTENTE
064400980703     C*--------------------
064500980703IF  3C     $ERR          IFEQ      '0'
064600980703     C                   MOVEL     V1CNZM        WNAR
064700980703     C                   MOVEL     *BLANKS       WDNAR
064800980703     C                   EXSR      CHKNAR
064900980703     C                   MOVEL     WNAR          V1CNZM
065000980703IF  4C     $ERR          IFEQ      '1'
065100980703     C                   MOVEL     MSG(10)       $MSG
065200980703     C                   EXSR      CARMSG
065300980703     C                   SETON                                        79
065400980703E   4C                   ENDIF
065500980703E   3C                   ENDIF
065600980703     C*--------------------
065700980703     C* CAP MITTENTE
065800980703     C*--------------------
065900980703IF  3C     $ERR          IFEQ      '0'
066000980703IF  4C     V1CCAM        IFGT      *BLANKS
066100090615     c                   clear                   tisi97ds
066200980703     C                   MOVEL     V1CNZM        WNAR
066300980703     C                   MOVEL     V1CCAM        WCAP
066400980703     C                   EXSR      CHKCAP
066500980703     C                   MOVEL     WNAR          V1CNZM
066600980703     C                   MOVEL     WCAP          V1CCAM
066700980703IF  5C     $ERR          IFEQ      '1'
066800980703     C                   MOVEL     MSG(11)       $MSG
066900980703     C                   EXSR      CARMSG
067000980703     C                   SETON                                        7980
067100980703E   5C                   ENDIF
067200980703E   4C                   ENDIF
067300980703E   3C                   ENDIF
067400980703     C*--------------------
067500980703     C* LOCALITA' MITTENTE
067600980703     C*--------------------
067700980703IF  3C     $ERR          IFEQ      '0'
067800980703IF  4C     V1CLOM        IFGT      *BLANKS
067900980703     C                   MOVEL     V1CNZM        WNAR
068000980703     C                   MOVEL     V1CCAM        WCAP
068100980703     C                   MOVEL     V1CLOM        WLOC
068200980703     C                   EXSR      CHKLOC
068300980703     C                   MOVEL     WNAR          V1CNZM
068400980703     C                   MOVEL     WCAP          V1CCAM
068500980703     C                   MOVEL     WLOC          V1CLOM
068600980703E   4C                   ENDIF
068700980703E   3C                   ENDIF
068800980703     C*--------------------
068900980703     C* CAP + LOCALITA' MITTENTE
069000980703     C*--------------------
069100980703IF  3C     $ERR          IFEQ      '0'
069200980703IF  4C     V1CCAM        IFEQ      *BLANKS
069300980703     C                   MOVEL     '1'           $ERR
069400980703     C                   MOVEL     MSG(11)       $MSG
069500980703     C                   EXSR      CARMSG
069600980703     C                   SETON                                        7980
069700980703E   4C                   ENDIF
069800980703E   3C                   ENDIF
069900980703IF  3C     $ERR          IFEQ      '0'
070000980703     C                   MOVEL     V1CNZM        WNAR
070100980703     C                   MOVEL     V1CCAM        WCAP
070200980703     C                   MOVEL     V1CLOM        WLOC
070300980703     C                   EXSR      CHKCLP
070400980703IF  4C     $ERR          IFEQ      '1'
070500980703     C                   MOVEL     MSG(14)       $MSG
070600980703     C                   EXSR      CARMSG
070700980703     C                   SETON                                        81
070800980703E   4C                   ENDIF
070900980703E   3C                   ENDIF
071000980703E   2C                   ENDIF
071100980703E   1C                   ENDIF
071200020225     C*--------------------
071300020225     C* NETWORK
071400020225     C*--------------------
071500020225IF  1C     $ERR          IFEQ      '0'
071600080912     c                   movel     svtbluni      ds15
071700090615    2C     V1CNAR        IFEQ      *BLANKS
071800090615    3C     V1CNTW        IFNE      *BLANKS
071900020225     C                   MOVEL     MSG(16)       $MSG
072000020225     C                   EXSR      CARMSG
072100020225     C                   SETON                                        6283
072200090615    3C                   ENDIF
072300090618     c                   clear                   Dntw
072400090618     c                   clear                   v1Dntw
072500120509     c                   if        savntw<>v1cntw
072600120509     c                   clear                   forzaIE
072700120509     c                   clear                   forzaCAPuti
072800120509     c                   movel     v1cntw        savntw
072900120509     c                   endif
073000090506     c
073100090615   x2C                   ELSE
073200020225     C                   EXSR      CHKNTW
073300090615IF  3C     $ERR          IFEQ      '1'
073400020225     C                   MOVEL     MSG(31)       $MSG
073500020225     C                   EXSR      CARMSG
073600020225     C                   SETON                                        83
073700090615   x3c                   else
073800090615     c* controllo cappario utilizzi particolari
073900090615     c                   clear                   tisi97ds
074000090615     c                   eval      i97ntw=v1cntw
074100090615     C                   MOVEL     V1CNAR        WNAR
074200090615     C                   MOVEL     V1CCAD        WCAP
074300090615     C                   MOVEL     V1CLOD        WLOC
074400090615     C                   EXSR      CHKCLP
074500090616     c
074600090616     c* errore forzabile per cappario utilizzi particolari
074700090616     c                   if        $err='1' and forzacaputi=' '
074800090615     C                   MOVEL     o13msg        $MSG
074900090615     C                   EXSR      CARMSG
075000090615     C                   SETON                                        83
075100090616     c                   eval      Forzacaputi='S'
075200090615E   4C                   ENDIF
075300090616     c
075400090615E   3C                   ENDIF
075500090615    2C                   ENDIF
075600020225E   1C                   ENDIF
075700020514     C*--------------------
075800020514     C* NAZIONE IN RELAZIONE AL NETWORK
075900020514     C*--------------------
076000020514IF  1C     $ERR          IFEQ      '0'
076100020514     C     V1CNAR        IFNE      *BLANKS
076200020514     C     V1CNTW        ANDNE     *BLANKS
076300020514     C* CONTROLLO X DPD
076400020514     C     V1CNTW        IFEQ      'DPD'
076500020514     C     §15LAD        ANDEQ     *ZEROS
076600020514     C                   SETON                                        6283
076700020514     C                   MOVEL     '1'           $ERR
076800020514     C                   MOVEL     MSG(32)       $MSG
076900020514     C                   EXSR      CARMSG
077000020514     C                   ENDIF
077100020514     C* CONTROLLO X FED-EX
077200020514     C     V1CNTW        IFEQ      'FED'
077300020514     C     §15LAF        ANDEQ     *ZEROS
077400020514     C                   SETON                                        6283
077500020514     C                   MOVEL     '1'           $ERR
077600020514     C                   MOVEL     MSG(32)       $MSG
077700020514     C                   EXSR      CARMSG
077800020514     C                   ENDIF
077900020514     C* CONTROLLO X FED-EX
078000020514     C     V1CNTW        IFEQ      'EEX'
078100020514     C     §15EEX        ANDNE     'S'
078200020514     C                   SETON                                        6283
078300020514     C                   MOVEL     '1'           $ERR
078400020514     C                   MOVEL     MSG(32)       $MSG
078500020514     C                   EXSR      CARMSG
078600020514     C                   ENDIF
078700020514     C                   ENDIF
078800020514E   1C                   ENDIF
078900050413     C*--------------------
079000050413     C* CONSEGNE PARTICOLARI
079100050413     C*--------------------
079200050413IF  1C     $ERR          IFEQ      '0'
079300050413     C                   MOVEL     V1CTC1        WTCX
079400050413     C                   MOVEL     *BLANKS       WDTCX
079500050413     C                   EXSR      CHKtcx
079600050413     C                   MOVEL     Wtcx          V1Ctc1
079700050413     C                   MOVEL     WDtcx         V1Dtc1
079800050413IF  2C     $ERR          IFEQ      '1'
079900050413     C                   MOVEL     MSG(34)       $MSG
080000050413     C                   EXSR      CARMSG
080100050413     C                   SETON                                        86
080200050413E   2C                   ENDIF
080300050413E   1C                   ENDIF
080400050413     c
080500050413IF  1C     $ERR          IFEQ      '0'
080600050413     C                   MOVEL     V1CTC2        WTCX
080700050413     C                   MOVEL     *BLANKS       WDTCX
080800050413     C                   EXSR      CHKtcx
080900050413     C                   MOVEL     Wtcx          V1Ctc2
081000050413     C                   MOVEL     WDtcx         V1Dtc2
081100050413IF  2C     $ERR          IFEQ      '1'
081200050413     C                   MOVEL     MSG(34)       $MSG
081300050413     C                   EXSR      CARMSG
081400050413     C                   SETON                                        87
081500050413E   2C                   ENDIF
081600050413E   1C                   ENDIF
081700050413     c* Devono essere <>
081800050413IF  1C     $ERR          IFEQ      '0'
081900050413     c                   if        v1ctc1 = v1ctc2 and v1ctc1<>' '
082000050413     c                   eval      $err='1'
082100050413     C                   MOVEL     MSG(35)       $MSG
082200050413     C                   EXSR      CARMSG
082300050413     C                   SETON                                        87
082400050413E   2C                   ENDIF
082500050413     c                   endif
082600960528     C***
082700960528     C* SIMULAZIONE TASSAZIONE
082800960528     C***
082900960729     C* SE I COLLI SONO = 0 NON ESEGUE LA TASSAZIONE
083000960729     C* SE I COLLI SONO > 0 RICERCA I DATI DI TASSAZIONE
083100960703     C*--------------------
083200000919     C* SE IL CLIENTE È VUOTO, IMPOSTA LA TARIFFA DI CARTELLO
083300960703     C*--------------------
083400120509 F  1C     $ERR          IFEQ      '0'
083500020313     C                   MOVEL     V1CKSC        DEPCART           5 0
083600020313IF  2C                   IF        V1CNCL  <> *ZEROS AND
083700020313     C                             (V1CKSC  = *ZEROS OR
083800020313     C                              DEPCART = 88888)
083900021104     C*
084000021104     C* REIMPOSTO I DATI RELATIVI ALLA TARIFFA DI CARTELLO SOLO SE NN TRATTASI DI FED-EX
084100120608     c*  o se cambiato il peso
084200160302     C                   IF        savt_NTW = 'FED' AND v1cNTW = 'FED'
084300120608     c                             and v1cpkb=savpkb
084400021104     C                   ELSE
084500020226     C* 1 VOLTA REPERITO/CONTROLLATO IL NETWORK DETERMINO LA TARIFFA DI CARTELLO
084600020226     C                   EXSR      CARTAR
084700160302     c                   move      oc7kscc       alfaksc           7
084800160302     c                   movel     oc7ctrc       alfactr           3
084900120608     c**                 if        alfaksc<>v1cksc or alfactr<>v1cctr
085000120608     c**                 eval      $err='1'
085100120608     C**                 ENDIF
085200160302     C                   MOVE      Oc7KSCc       V1CKSC
085300160302     C                   MOVEL     Oc7CTRc       V1CCTR
085400120608     C                   MOVEL     V1CCTR        WCTR
085500120608     C                   EXSR      CHKCTR
085600020226     C***                MOVEL     *ZEROS        V1CCTR
085700120608     c                   eval      savpkb=v1cpkb
085800160302     c                   eval      savt_ntw=v1cntw
085900120608     c
086000120608     C                   ENDIF
086100960729E   2C                   ENDIF
086200960729E   1C                   ENDIF
086300120509     C*----------------------------
086400120509     C* COLLI e Peso rispetto a DPD
086500120509     C*----------------------------
086600120509 F  1C     $ERR          IFEQ      '0'
086700120509      * OBBLIGATORIO 1 COLLO
086800120509     C     §OGNTW        IFEQ      'DPD'
086900120509     C     v1cntw        oreq      'DPD'
087000120509     C     V1CNCL        ifne      1
087100120509     C                   MOVEL     '1'           $ERR
087200120509     C                   MOVEL     MSG(47)       $MSG
087300120509     C                   EXSR      CARMSG
087400120509     C                   SETON                                        67
087500120509     C                   ENDIF
087600120509     C                   ENDIF
087700120509    1C                   ENDIF
087800120509      * Massimo peso inseribile --> da tabella
087900120509 F  1C     $ERR          IFEQ      '0'
088000120514     C                   if        (v1cntw='DPD' or §ogntw='DPD')
088100120509     c                             and v1cpkb>§3ipkd
088200120509     C                   MOVEL     '1'           $ERR
088300120509     C                   MOVEL     MSG(48)       $MSG
088400120509     c                   eval      $msg = %trim($msg) + ' ' +
088500120509     c                             %trim(%editc(§3ipkd:'2'))+
088600120509     c                             ' Kg.'
088700120509     C                   EXSR      CARMSG
088800120509     C                   SETON                                        68
088900120509     C                   ENDIF
089000120509    1c                   endif
089100960528     C*--------------------
089200960528     C* PESO
089300960528     C*--------------------
089400960528IF  1C     $ERR          IFEQ      '0'
089500960729IF  2C     V1CNCL        IFGT      *ZEROS
089600960729IF  3C     V1CPKB        IFLE      *ZEROS
089700960528     C                   MOVEL     '1'           $ERR
089800960528     C                   MOVEL     MSG(18)       $MSG
089900960528     C                   EXSR      CARMSG
090000960528     C                   SETON                                        68
090100960729E   3C                   ENDIF
090200960729X   2C                   ELSE
090300960729     C                   Z-ADD     *ZEROS        V1CPKB
090400960729E   2C                   ENDIF
090500960528E   1C                   ENDIF
090600960528     C*--------------------
090700960528     C* VOLUME
090800960528     C*--------------------
090900960528IF  1C     $ERR          IFEQ      '0'
091000960729IF  2C     V1CNCL        IFGT      *ZEROS
091100960729IF  3C     V1CVLF        IFLE      *ZEROS
091200960528     C                   MOVEL     '1'           $ERR
091300960528     C                   MOVEL     MSG(19)       $MSG
091400960528     C                   EXSR      CARMSG
091500960528     C                   SETON                                        69
091600960729E   2C                   ENDIF
091700960729X   2C                   ELSE
091800960729     C                   Z-ADD     *ZEROS        V1CVLF
091900960528E   2C                   ENDIF
092000960528E   1C                   ENDIF
092100110923     C*--------------------
092200110923     C* Limiti colli/peso/volume
092300110923     C*--------------------
092400110923IF  1C     $ERR          IFEQ      '0'
092500110923     c*
092600110923     c* campi per la forzatura del messaggio di errore --> pulizia a cambio di tipo servizio
092700110923     c                   if        v1ctsp<>savtsp
092800110923     c                   clear                   wncl_pre
092900110923     c                   clear                   wpes_pre
093000110923     c                   clear                   wvol_pre
093100110923     c                   eval      savtsp=v1ctsp
093200110923     c                   endif
093300110923     c*
093400110923     c                   clear                   trul73ds
093500110923     c                   eval      i73tsp=v1ctsp
093600110923     c                   eval      i73psk=v1cpkb
093700110923     c                   eval      i73vmc=v1cvlf
093800110923     c                   eval      i73ncl=v1cncl
093900110923     c                   call      'TRUL73R'
094000110923     c                   parm                    trul73ds
094100110923     c                   select
094200110923     c* colli
094300110927     c                   when      o73fclm='1' and v1cncl<>wncl_pre
094400110923     C                   MOVEL     '1'           $ERR
094500110923     c                   eval      $msg=msg(46)
094600110923     C                   EXSR      CARMSG
094700110923     c                   seton                                        67
094800110923     c                   eval      wncl_pre=v1cncl
094900110923     c* peso kg
095000110927     c                   when      O73FKGM='1' and v1cpkb<>wpes_pre
095100110923     C                   MOVEL     '1'           $ERR
095200110923     c                   eval      $msg=msg(46)
095300110923     C                   EXSR      CARMSG
095400110923     c                   seton                                        68
095500110923     c                   eval      wpes_pre=v1cpkb
095600110923     c*volume mc
095700110927     c                   when      O73FMCM='1' and v1cvlf<>wvol_pre
095800110923     C                   MOVEL     '1'           $ERR
095900110923     c                   eval      $msg=msg(46)
096000110923     C                   EXSR      CARMSG
096100110923     c                   seton                                        69
096200110923     c                   eval      wvol_pre=v1cvlf
096300110923     c                   endsl
096400110923E   1C                   ENDIF
096500960614     C*--------------------
096600960614     C* CLIENTE
096700960614     C*--------------------
096800960703IF  1C     $ERR          IFEQ      '0'
096900960729IF  2C     V1CNCL        IFGT      *ZEROS
097000960614     C                   MOVEL     V1CKSC        WKSC
097100960614     C                   MOVEL     *BLANKS       WDKSC
097200960614     C                   EXSR      CHKKSC
097300960614     C                   MOVEL     WKSC          V1CKSC
097400960614     C                   MOVEL     WDKSC         V1DKSC
097500960729IF  3C     $ERR          IFEQ      '1'
097600960614     C                   MOVEL     MSG(20)       $MSG
097700960614     C                   EXSR      CARMSG
097800960614     C                   SETON                                        70
097900960729E   3C                   ENDIF
098000960729X   2C                   ELSE
098100960729     C                   MOVEL     *ZEROS        V1CKSC
098200960729E   2C                   ENDIF
098300960703E   1C                   ENDIF
098400960614     C*--------------------
098500960614     C* TARIFFA
098600960614     C*--------------------
098700960703IF  1C     $ERR          IFEQ      '0'
098800090429     c
098900960729IF  2C     V1CNCL        IFGT      *ZEROS
099000960614     C                   MOVEL     V1CCTR        WCTR
099100960614     C                   EXSR      CHKCTR
099200960729IF  3C     $ERR          IFEQ      '1'
099300960729E   3C                   ENDIF
099400960729X   2C                   ELSE
099500960729     C                   MOVEL     *ZEROS        V1CCTR
099600960729E   2C                   ENDIF
099700960703E   1C                   ENDIF
099800010823     C*--------------------
099900960925     C* C/ASSEGNO
100000010823     C*--------------------
100100010823     C*=======
100200010823     C* DIVISA E/O TIPO INCASSO IMPOSTATI MA MANCA IMPORTO CONTRASSEGNO
100300010823     C*=======
100400010823IF  1C     $ERR          IFEQ      '0'
100500010823IF  2C     V1CVCA        IFNE      *BLANKS
100600010823     C     V1CTIC        ORNE      *BLANKS
100700010823IF  3C     V1CCAS        IFEQ      *ZEROS
100800010823     C                   MOVEL     '1'           $ERR
100900010823     C                   MOVEL     MSG(24)       $MSG
101000010823     C                   EXSR      CARMSG
101100010823     C                   SETON                                        72
101200010823E   3C                   ENDIF
101300010823E   2C                   ENDIF
101400010823E   1C                   ENDIF
101500010823     C*
101600010823     C*=======
101700010823     C* DIVISA
101800010823     C*=======
101900010823IF  1C     $ERR          IFEQ      '0'
102000010823IF  2C     V1CCAS        IFGT      *ZEROS
102100010823     C                   MOVEL     V1CVCA        WDIV
102200010823     C                   EXSR      CHKDIV
102300010823     C                   MOVEL     WDIV          V1CVCA
102400090616IF  3C     $ERR          IFEQ      '1'
102500010829     C     V1CVCA        OREQ      *BLANKS
102600010823     C                   MOVEL     MSG(22)       $MSG
102700010823     C                   EXSR      CARMSG
102800010823     C                   SETON                                        73
102900090616E   3C                   ENDIF
103000010823     C*=======
103100010823     C* TIPO INCASSO
103200010823     C*=======
103300090616IF  3C     $ERR          IFEQ      '0'
103400010823     C                   MOVEL     V1CTIC        WTIC
103500010823     C                   MOVEL     *BLANKS       WUDE
103600010823     C                   EXSR      CHKTIC
103700010823     C                   MOVEL     WTIC          V1CTIC
103800090616IF  4C     $ERR          IFEQ      '1'
103900010823     C                   MOVEL     MSG(23)       $MSG
104000010823     C                   EXSR      CARMSG
104100010823     C                   SETON                                        74
104200090616E   4C                   ENDIF
104300090616     c
104400090617E   3C                   ENDIF
104500010823E   2C                   ENDIF
104600960926E   1C                   ENDIF
104700960926     C*--------------------
104800960926     C* VALUTA + IMPORTO
104900960926     C*--------------------
105000960926IF  1C     $ERR          IFEQ      '0'
105100960926IF  2C     WFDC          IFNE      'S'
105200960926     C                   MOVE      V1CCAS        WN33              3 3
105300960926IF  3C     WN33          IFNE      *ZEROS
105400960926     C                   MOVEL     '1'           $ERR
105500960926     C                   MOVEL     MSG(25)       $MSG
105600960926     C                   EXSR      CARMSG
105700960926     C                   SETON                                        7273
105800960926E   3C                   ENDIF
105900960926E   2C                   ENDIF
106000960926E   1C                   ENDIF
106100090617
106200960926     C*---
106300960926     C* ASSICURAZIONE
106400960926     C*---
106500960926     C*--------------------
106600960926     C* VALUTA
106700960926     C*--------------------
106800010823IF  1C     $ERR          IFEQ      '0'
106900010823IF  2C     V1CIAS        IFGT      *ZEROS
107000960926     C                   MOVEL     *BLANKS       WFDC
107100960926     C                   MOVEL     V1CVAS        WDIV
107200960926     C                   EXSR      CHKDIV
107300960926     C                   MOVEL     WDIV          V1CVAS
107400010829IF  5C     $ERR          IFEQ      '1'
107500010829     C     V1CVAS        OREQ      *BLANKS
107600960926     C                   MOVEL     MSG(22)       $MSG
107700960926     C                   EXSR      CARMSG
107800960926     C                   SETON                                        76
107900010829E   5C                   ENDIF
108000010829E   2C                   ENDIF
108100010829E   1C                   ENDIF
108200960926     C*--------------------
108300960926     C* IMPORTO
108400960926     C*--------------------
108500960926IF  1C     $ERR          IFEQ      '0'
108600960926IF  2C     V1CVAS        IFNE      *BLANKS
108700960926IF  3C     V1CIAS        IFEQ      *ZEROS
108800960926     C                   MOVEL     '1'           $ERR
108900960926     C                   MOVEL     MSG(27)       $MSG
109000960926     C                   EXSR      CARMSG
109100960926     C                   SETON                                        75
109200960926E   3C                   ENDIF
109300960926E   2C                   ENDIF
109400960926E   1C                   ENDIF
109500960926     C*--------------------
109600960926     C* VALUTA + IMPORTO
109700960926     C*--------------------
109800960926IF  1C     $ERR          IFEQ      '0'
109900960926IF  2C     WFDC          IFNE      'S'
110000960926     C                   MOVE      V1CIAS        WN33              3 3
110100960926IF  3C     WN33          IFNE      *ZEROS
110200960926     C                   MOVEL     '1'           $ERR
110300960926     C                   MOVEL     MSG(25)       $MSG
110400960926     C                   EXSR      CARMSG
110500960926     C                   SETON                                        7576
110600960926E   3C                   ENDIF
110700960926E   2C                   ENDIF
110800960926E   1C                   ENDIF
110900021106     C*--------------------
111000021106     C* CONSEGNA DDT
111100021106     C*--------------------
111200021106IF  1C     $ERR          IFEQ      '0'
111300021106IF  2C     V1CDDT        IFEQ      'S'
111400021106     C     V1CDDT        OREQ      'N'
111500021106X   2C                   ELSE
111600021106     C                   MOVEL     '1'           $ERR
111700021106     C                   MOVEL     MSG(15)       $MSG
111800021106     C                   EXSR      CARMSG
111900021106     C                   SETON                                        85
112000021106E   2C                   ENDIF
112100021106E   1C                   ENDIF
112200110817     C*----------------------
112300110817     C* EMAIL AL DESTINATARIO
112400110817     C*----------------------
112500110817IF  1C     $ERR          IFEQ      '0'
112600110817IF  2C     V1CEMD        IFEQ      'S'
112700110817     C     V1CEMD        OREQ      'N'
112800130925     C     V1CEMD        OREQ      'M'
112900110817X   2C                   ELSE
113000110817     C                   MOVEL     '1'           $ERR
113100110817     C                   MOVEL     MSG(15)       $MSG
113200110817     C                   EXSR      CARMSG
113300110817     C                   SETON                                        88
113400110817E   2C                   ENDIF
113500110817E   1C                   ENDIF
113600960926     C*
113700960926     C                   ENDSR
113800000000     C*------------------------------------------------------------------------*
113900000000     C* CHKDA8 - CONTROLLA UNA DATA LUNGA 8 (G/M/A) E LA INVERTE (A/M/G)
114000000000     C*------------------------------------------------------------------------*
114100000000     C     CHKDA8        BEGSR
114200000000     C*
114300000000     C     *LIKE         DEFINE    G08DAT        WDA8
114400000000     C     *LIKE         DEFINE    G08INV        DA8W
114500000000     C*
114600000000     C                   Z-ADD     WDA8          G08DAT
114700000000     C                   Z-ADD     *ZEROS        G08INV
114800000000     C                   MOVEL     '0'           G08ERR
114900000000     C                   CALL      'XSRDA8'
115000000000     C                   PARM                    WLBDA8
115100000000     C                   Z-ADD     G08DAT        WDA8
115200000000IF  1C     G08ERR        IFEQ      '1'
115300000000     C                   MOVEL     '1'           $ERR
115400000000X   1C                   ELSE
115500000000     C                   Z-ADD     G08INV        DA8W
115600000000E   1C                   ENDIF
115700000000     C*
115800000000     C                   ENDSR
115900000000     C*------------------------------------------------------------------------*
116000000000     C* CHKLNP - CONTROLLA LA FILIALE
116100000000     C*------------------------------------------------------------------------*
116200000000     C     CHKLNP        BEGSR
116300000000     C*
116400000000     C     *LIKE         DEFINE    V1CLNP        WLNP
116500000000     C     *LIKE         DEFINE    V1DLNP        WDLNP
116600000000     C*
116700970704     C* RICHIESTO ELENCO
116800000000     C     '?'           SCAN      WLNP                                   99
116900000000IF  1C     *IN99         IFEQ      *ON
117000000000     C                   MOVEL     *BLANKS       §TIP              1
117100960926     C                   MOVEL     *BLANKS       §FIL              3
117200960926     C                   MOVE      *BLANKS       §DEC             25
117300971203     C                   MOVE      *BLANKS       §PRA              3
117400971203     C                   CALL      'TNSD24R'
117500960926     C                   PARM                    §FIL
117600000000     C                   PARM                    §TIP
117700960926     C                   PARM                    §DEC
117800971203     C                   PARM                    §PRA
117900960926     C                   MOVEL     §FIL          WLNP
118000960926     C                   MOVEL     §DEC          WDLNP
118100970704     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
118200970704E   1C                   ENDIF
118300000000     C*
118400970704     C* SOLO CARATTERI NUMERICI
118500000000     C                   SETOFF                                       28
118600000000     C                   TESTN                   WLNP                 28
118700970704IF  1C     *IN28         IFEQ      *ON                                          DA TESTN E'NUMERO
118800000000     C                   MOVE      WLNP          W001              1
118900970704IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
119000000000     C                   MOVEL     '1'           $ERR
119100970704E   2C                   ENDIF
119200970704X   1C                   ELSE                                                   DA TESTN E'UN NUMERO
119300000000     C                   MOVEL     '1'           $ERR
119400970704E   1C                   ENDIF
119500000000     C*
119600970704     C* CONTROLLA ESISTENZA CODICE
119700970704IF  1C     $ERR          IFEQ      '0'
119800000000     C                   MOVEL     WLNP          KORFIL
119900120509     c                   clear                   og143
120000000000     C     KEYORG        CHAIN     AZORG01L                           99
120100970704IF  2C     *IN99         IFEQ      *OFF
120200000000     C     ORGFVA        ANDEQ     *BLANKS
120300000000     C     ORGFAG        ANDNE     'V'
120400000000     C                   MOVEL     ORGDES        WDLNP
120500120509     C                   MOVEL     ORGDE3        OG143
120600970704X   2C                   ELSE
120700000000     C                   MOVEL     '1'           $ERR
120800000000     C                   MOVEL     *ALL'*'       WDLNP
120900970704E   2C                   ENDIF
121000000000E   1C                   ENDIF
121100000000     C*
121200000000     C                   ENDSR
121300000000     C*------------------------------------------------------------------------*
121400000000     C* CHKNAR - CONTROLLA LA NAZIONE
121500000000     C*------------------------------------------------------------------------*
121600000000     C     CHKNAR        BEGSR
121700000000     C*
121800000000     C     *LIKE         DEFINE    V1CNAR        WNAR
121900000000     C     *LIKE         DEFINE    V1DNAR        WDNAR
122000000000     C*
122100970704     C* RICHIESTO ELENCO
122200000000     C     '?'           SCAN      WNAR                                   99
122300000000IF  1C     *IN99         IFEQ      *ON
122400020919     C                   MOVEL     1             §KUT              1 0
122500000000     C                   MOVEL     '15'          §TAB              2
122600000000     C                   MOVEL     *BLANKS       §KEY              8
122700960926     C                   MOVEL     *BLANKS       §DES             30
122800000000     C                   CALL      'X§TABER'
122900000000     C                   PARM                    §KUT
123000000000     C                   PARM                    §TAB
123100000000     C                   PARM                    §KEY
123200960926     C                   PARM                    §DES
123300000000     C                   MOVEL     §KEY          WNAR
123400960926     C                   MOVEL     §DES          WDNAR
123500000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
123600970704E   1C                   ENDIF
123700000000     C*
123800970704     C* CONTROLLA ESISTENZA CODICE
123900020919     C                   MOVEL     1             KTBKUT
124000000000     C                   MOVEL     '15'          KTBCOD
124100000000     C                   MOVEL     *BLANKS       KTBKEY
124200000000     C                   MOVEL     WNAR          KTBKEY
124300000000     C     KEYTAB        CHAIN     TABEL00F                           99
124400970704IF  1C     *IN99         IFEQ      *OFF
124500000000     C     TBLFLG        ANDEQ     *BLANKS
124600000000     C                   MOVEL     TBLUNI        DS15
124700000000     C                   MOVEL     §15DES        WDNAR
124800970704X   1C                   ELSE
124900000000     C                   MOVEL     '1'           $ERR
125000000000     C                   MOVEL     *ALL'*'       WDNAR
125100000000E   1C                   ENDIF
125200000000     C*
125300000000     C                   ENDSR
125400020225     C*------------------------------------------------------------------------*
125500020225     C* CHKNTW - CONTROLLA IL NETWORK
125600020225     C*------------------------------------------------------------------------*
125700020225     C     CHKNTW        BEGSR
125800020225     C*
125900020225     C* RICHIESTO ELENCO
126000020225     C     '?'           SCAN      V1CNTW                                 99
126100020225     C* SCELTA VALORE DA TABELLA
126200020225IF  1C     *IN99         IFEQ      *ON
126300020925     C                   CLEAR                   TIBS02DS
126400020225     C                   MOVEL     'NTW'         T02COD
126500020225     C                   MOVEL     KNSIF         T02SIF
126600020225     C                   MOVEL     'R'           T02MOD
126700020225     C                   CALL      'TIBS02R'
126800020225     C                   PARM                    KPJBA
126900020925     C                   PARM                    TIBS02DS
127000020225     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
127100020225     C*
127200020225     C                   ELSE
127300020225     C* VERIFICA VALORE IN TABELLA
127400020925     C                   CLEAR                   TIBS02DS
127500020225     C                   MOVEL     'NTW'         T02COD
127600020225     C                   MOVEL     KNSIF         T02SIF
127700020225     C                   MOVEL     V1CNTW        T02KE1
127800020225     C                   MOVEL     'C'           T02MOD
127900020225     C                   CALL      'TIBS02R'
128000020225     C                   PARM                    KPJBA
128100020925     C                   PARM                    TIBS02DS
128200020225E   1C                   ENDIF
128300020225     C*
128400020225     C     T02ERR        IFEQ      *BLANKS
128500020225     C                   MOVEL     T02UNI        DNTW
128600020226     C                   MOVEL     T02KE1        V1CNTW
128700020226     C                   MOVEL     §NTWDES       V1DNTW
128800020225     C     §NTWFIE       IFEQ      'E'
128900020225     C                   ELSE
129000020225     C                   MOVEL     '1'           $ERR
129100020225     C                   ENDIF
129200020225     C                   ELSE
129300020225     C                   MOVEL     '1'           $ERR
129400020225     C                   ENDIF
129500090506     c                   if        savntw<>v1cntw
129600090506     c                   clear                   forzaIE
129700090616     c                   clear                   forzaCAPuti
129800090506     c                   movel     v1cntw        savntw
129900090506     c                   endif
130000020225     C*
130100020225     C                   ENDSR
130200050413     C*------------------------------------------------------------------------*
130300050413     C* CHKTCx - CONTROLLA consegna particolare
130400050413     C*------------------------------------------------------------------------*
130500050413     C     CHKTCx        BEGSR
130600050413    1C     wtcx          IFNE      ' '
130700050413     C* RICERCA
130800050413    2C     wtcx          IFEQ      '?'
130900050413     C                   MOVEL     1             §KUT              1 0
131000050413     C                   MOVEL     '1P'          §TAB              2
131100050413     C                   MOVEL     *BLANKS       §KEY              8
131200050413     C                   MOVEL     *BLANKS       §DES             30
131300050413     C                   CALL      'X§TABER'
131400050413     C                   PARM                    §KUT
131500050413     C                   PARM                    §TAB
131600050413     C                   PARM                    §KEY
131700050413     C                   PARM                    §DES
131800050413     C                   MOVEL     §KEY          WTcx
131900050413     C                   MOVEL     §DES          WDTCx
132000050413     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
132100050413    2C                   END
132200050413     C* CONTROLLO
132300050413     C                   MOVEL     1             KTBKUT
132400050413     C                   MOVEL     '1P'          KTBCOD
132500050413     C                   MOVEL(P)  WTCx          KTBKEY
132600050413     C     KEYTAB        CHAIN     TABEL00F                           99
132700050413IF  2C     *IN99         IFEQ      *OFF
132800050413     C     TBLFLG        ANDEQ     *BLANKS
132900050413     C                   MOVEL     tbluni        WDTCx
133000050413     C                   MOVEL     tbluni        ds1p
133100050413X   2C                   ELSE
133200050413     C                   MOVEL     '1'           $ERR
133300050413     C                   MOVEL     *ALL'*'       WDTCx
133400050413E   2C                   ENDIF
133500050413     c* Se non ' utilizzza<bile errore
133600050413     c                   if        $err='0' and
133700050413     c                             §1puar=' ' and §1pupr=' '
133800050413     C                   MOVEL     '1'           $ERR
133900050413     C                   MOVEL     *ALL'*'       WDTCx
134000050413     c                   endif
134100050413E   1C                   ENDIF
134200050413     c
134300050413     c                   endsr
134400000000     C*------------------------------------------------------------------------*
134500970704     C* CHKCAP - CONTROLLA IL CAP
134600000000     C*------------------------------------------------------------------------*
134700000000     C     CHKCAP        BEGSR
134800000000     C*
134900000000     C     *LIKE         DEFINE    V1CCAD        WCAP
135000970704     C*
135100970704     C* RICHIESTO ELENCO
135200970704     C                   SETOFF                                           99
135300970704     C     '?'           SCAN      WCAP                                   99
135400970704IF  1C     *IN99         IFEQ      *ON
135500970704     C                   CLEAR                   DSSI00
135600970704     C                   MOVEL     'C01'         D00OP0
135700970704     C                   Z-ADD     DSPV1C        D00DRI
135800970704     C                   CALL      'TISI02R'
135900000000     C                   PARM                    KPJBA
136000970704     C                   PARM                    DSSI00
136100970704IF  2C     D00ERR        IFNE      '1'
136200970704     C                   MOVEL     D00NAR        WNAR                           *NAZIONE
136300970704     C                   MOVEL     D00CAP        WCAP                           *CAP
136400970704E   2C                   ENDIF
136500000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
136600970704E   1C                   ENDIF
136700970704     C*
136800970704     C* CONTROLLA ESISTENZA CODICE
136900090615     C                   CLEAR                   fnlv13ds
137000090615     C                   CLEAR                   DSSI95
137100970704     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
137200970704     C                   MOVEL     '3'           I95TCN                         *CONTROLLO
137300970704     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
137400120802
137500120802     c                   if        wnar='IRL'
137600120802     C                   MOVEL     'EIRE'        I95CAP                         *CAP
137700120802     c                   else
137800120802     C                   MOVEL     WCAP          I95CAP                         *CAP
137900120802     c                   endif
138000120802     c
138100970704     C                   Z-ADD     DSPV1C        I95DAT                         *DATA RIFERIMENTO
138200020225     C                   IF        V1CNTW = 'FED'
138300020225     C                   MOVEL     'S'           I95FI1
138400020225     C                   ENDIF
138500090615     C* ERRORE AFFIDABILITA = 0
138600090615     C                   MOVEL     ' '           I13AF0
138700090615     C                   CALL      'FNLV13R'
138800090615     C                   PARM                    KPJBA
138900090615     C                   PARM                    fnlv13ds
139000970704     C                   PARM                    DSSI95
139100090615     C                   PARM      ' '           flgbac            1
139200090615     C                   PARM                    tisi97ds
139300090615     c
139400970704IF  1C     O95ERR        IFNE      *BLANKS
139500090615     c     o13err        orne      *blanks
139600090629     c     o13rop        andeq     *blanks
139700970704     C                   MOVEL     '1'           $ERR
139800970704E   1C                   ENDIF
139900000000     C*
140000000000     C                   ENDSR
140100970704     C*------------------------------------------------------------------------*
140200970704     C* CHKLOC - CONTROLLA LA LOCALITA'
140300970704     C*------------------------------------------------------------------------*
140400970704     C     CHKLOC        BEGSR
140500970704     C*
140600970704     C     *LIKE         DEFINE    V1CLOD        WLOC
140700970704     C*
140800970704     C* RICHIESTO ELENCO
140900970704     C                   SETOFF                                           99
141000970704     C     '?'           SCAN      WLOC                                   99
141100970704IF  1C     *IN99         IFEQ      *ON
141200970704     C                   CLEAR                   DSSI00
141300970704     C                   MOVEL     'L01'         D00OP0
141400970704     C                   MOVEL     WNAR          D00NAR
141500970704     C                   MOVEL     WCAP          D00CAP
141600970704     C                   Z-ADD     DSPV1C        D00DRI
141700970704     C                   CALL      'TISI30R'
141800970704     C                   PARM                    KPJBA
141900970704     C                   PARM                    DSSI00
142000970704IF  2C     D00ERR        IFNE      '1'
142100970704     C                   MOVEL     D00NAR        WNAR                           *NAZIONE
142200970704     C                   MOVEL     D00CAP        WCAP                           *CAP
142300970704     C                   MOVEL     D00LOC        WLOC                           *LOCALITA'
142400970704E   2C                   ENDIF
142500970704     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
142600970704E   1C                   ENDIF
142700970704     C*
142800970704     C                   ENDSR
142900970704     C*------------------------------------------------------------------------*
143000970704     C* CHKCLP - CONTROLLA CAP + LOCALITA' + PROVINCIA
143100970704     C*------------------------------------------------------------------------*
143200970704     C     CHKCLP        BEGSR
143300970704     C*
143400970704     C* CONTROLLA ESISTENZA CODICE
143500970704     C                   CLEAR                   DSSI95
143600970704     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
143700970704     C                   MOVEL     '3'           I95TCN                         *CONTROLLO
143800970704     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
143900120802     c                   if        wnar='IRL'
144000120802     C                   MOVEL     'EIRE'        I95CAP                         *CAP
144100120802     c                   else
144200120802     C                   MOVEL     WCAP          I95CAP                         *CAP
144300120802     c                   endif
144400970704     C                   MOVEL     WLOC          I95LOC                         *LOCALITA'
144500970704     C                   Z-ADD     DSPV1C        I95DAT                         *DATA RIFERIMENTO
144600020225     C                   IF        V1CNTW = 'FED'
144700020225     C                   MOVEL     'S'           I95FI1
144800020225     C                   ENDIF
144900090615     c* C/ASS
145000090615     C     v1ccas        IFgt      0
145100090615     C                   MOVEL     'S'           I95FCA
145200090615     C                   ENDIF
145300090615     c* Tipo bolla
145400090615     C                   MOVEL     v1ctpo        I95TPO
145500090615     C* ERRORE AFFIDABILITA = 0
145600090615     C                   MOVEL     ' '           I13AF0
145700090615     C                   CALL      'FNLV13R'
145800090615     C                   PARM                    KPJBA
145900090615     C                   PARM                    fnlv13ds
146000090615     C                   PARM                    DSSI95
146100090615     C                   PARM      ' '           flgbac            1
146200090615     C                   PARM                    tisi97ds
146300090615     c
146400090615     c* Errore per il network
146500090616    0c                   if        i97ntw<>*blanks
146600090617     c                   movel     o97flo        dcppflo
146700090617     c
146800090617     c                   select
146900090617    1c                   when      forzacaputi<>' '
147000090617     c
147100090617    1c                   when      o13err<>*blanks
147200090615     C                   MOVEL     '1'           $ERR
147300090618    2c                   if        §cppall=' 'and
147400090618     c                             §cpppas<>' ' and §cppcas='S'
147500090618     c                   eval      o13msg=msg(44)
147600090618    2c                   endif
147700090618     c
147800090617   x1c                   other
147900090617    2c                   if        §cpppas='O' and v1ctpo='A'
148000090616     C                   MOVEL     '1'           $ERR
148100090617     c                   eval      o13msg=msg(45)
148200090618     c                   else
148300090618     c                   eval       o13msg=%trim(o13msg)+':ENTR forza'
148400090616    2c                   endif
148500090617    1c                   endsl
148600090616     c
148700090615   x0c                   else
148800090615     c
148900090615     c* Errore sulla località
149000970704IF  1C     O95ERR        IFNE      *BLANKS
149100120802     c
149200090615IF   C     o13err        orNE      *BLANKS
149300090629     c     o13rop        andeq     *blanks
149400120802
149500970714     C     O95LIA        OREQ      '1'
149600120802     c
149700120802     C     WLOC          ORNE      *BLANKS
149800120802     C     Wnar          andeq     *BLANKS
149900970714     C     O95LID        ANDLT     '3'
150000970704     C                   MOVEL     '1'           $ERR
150100970704E   1C                   ENDIF
150200090615E   0C                   ENDIF
150300970704     C*
150400970704     C                   ENDSR
150500000000     C*------------------------------------------------------------------------*
150600000000     C* CHKTSP - CONTROLLA IL TIPO SERVIZIO
150700000000     C*------------------------------------------------------------------------*
150800000000     C     CHKTSP        BEGSR
150900000000     C*
151000000000     C     *LIKE         DEFINE    V1CTSP        WTSP
151100000000     C     *LIKE         DEFINE    V1DTSP        WDTSP
151200000000     C*
151300970704     C* RICHIESTO ELENCO
151400000000     C     '?'           SCAN      WTSP                                   99
151500000000IF  1C     *IN99         IFEQ      *ON
151600020919     C                   MOVEL     1             §KUT              1 0
151700000000     C                   MOVEL     '5E'          §TAB              2
151800000000     C                   MOVEL     *BLANKS       §KEY              8
151900960926     C                   MOVEL     *BLANKS       §DES             30
152000000000     C                   CALL      'X§TABER'
152100000000     C                   PARM                    §KUT
152200000000     C                   PARM                    §TAB
152300000000     C                   PARM                    §KEY
152400960926     C                   PARM                    §DES
152500000000     C                   MOVEL     §KEY          WTSP
152600960926     C                   MOVEL     §DES          WDTSP
152700000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
152800970704E   1C                   ENDIF
152900000000     C*
153000970704     C* CONTROLLA ESISTENZA CODICE
153100020919     C                   MOVEL     1             KTBKUT
153200000000     C                   MOVEL     '5E'          KTBCOD
153300000000     C                   MOVEL     *BLANKS       KTBKEY
153400000000     C                   MOVEL     WTSP          KTBKEY
153500000000     C     KEYTAB        CHAIN     TABEL00F                           99
153600970704IF  1C     *IN99         IFEQ      *OFF
153700000000     C     TBLFLG        ANDEQ     *BLANKS
153800000000     C                   MOVEL     TBLUNI        DS5E
153900000000     C                   MOVEL     §5EDES        WDTSP
154000970704X   1C                   ELSE
154100000000     C                   MOVEL     '1'           $ERR
154200000000     C                   MOVEL     *ALL'*'       WDTSP
154300000000E   1C                   ENDIF
154400000000     C*
154500960703     C                   ENDSR
154600960926     C*------------------------------------------------------------------------*
154700960926     C* CHKDIV - CONTROLLA LA DIVISA
154800960926     C*------------------------------------------------------------------------*
154900960926     C     CHKDIV        BEGSR
155000960926     C*
155100960926     C     *LIKE         DEFINE    V1CVCA        WDIV
155200960926     C     *LIKE         DEFINE    §CVFDC        WFDC
155300010823     C     *LIKE         DEFINE    §CVDVA        WDVA
155400010823     C     *LIKE         DEFINE    §CWDLC        WDLC
155500960926     C*
155600970704     C* RICHIESTA ELENCO
155700960926     C     '?'           SCAN      WDIV                                   99
155800960926IF  1C     *IN99         IFEQ      *ON
155900020919     C                   Z-ADD     1             §KUT
156000960926     C                   MOVEL     'CV'          §COD
156100960926     C                   MOVEL     *BLANKS       §KEY
156200960926     C                   MOVEL     *BLANKS       §DES
156300960926     C                   CALL      'X§TABER'
156400960926     C                   PARM                    §KUT              1 0
156500960926     C                   PARM                    §COD              2
156600960926     C                   PARM                    §KEY              8
156700960926     C                   PARM                    §DES             30
156800960926     C                   MOVEL     §KEY          WDIV
156900960926     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
157000960926E   1C                   ENDIF
157100960926     C*
157200960926     C* CONTROLLA ESISTENZA CODICE
157300020919     C                   Z-ADD     1             KTBKUT
157400960926     C                   MOVEL     'CV'          KTBCOD
157500960926     C                   MOVEL(P)  WDIV          KTBKEY
157600960926     C     KEYTAB        CHAIN     TABEL00F                           99
157700960926IF  1C     *IN99         IFEQ      *ON
157800960926     C     TBLFLG        OREQ      '*'
157900960926     C                   MOVEL     '1'           $ERR
158000960926X   1C                   ELSE
158100960926     C                   MOVEL     TBLUNI        DSCV
158200960926     C                   MOVEL     §CVFDC        WFDC                           *DIVISA CON IMP.DEC.
158300010823     C                   MOVEL     §CVDVA        WDVA                           *S=SI DIVISA ALTERNA
158400010823     C                   ENDIF
158500010823     C*
158600010823     C* REPERISCO GIA' QUI L'ESTENSIONE DELLA TABELLA CV
158700020919     C                   Z-ADD     1             KTBKUT
158800010823     C                   MOVEL     'CW'          KTBCOD
158900010823     C                   MOVEL(P)  WDIV          KTBKEY
159000010823     C     KEYTAB        CHAIN     TABEL00F                           99
159100010823IF  1C     *IN99         IFEQ      *ON
159200010823     C     TBLFLG        OREQ      '*'
159300010823     C                   MOVEL     '1'           $ERR
159400010823X   1C                   ELSE
159500010823     C                   MOVEL     TBLUNI        DSCW
159600010823     C                   Z-ADD     §CWDLC        WDLC                           *DATA LIMITE CONTABI
159700010823     C                   ENDIF
159800960926     C*
159900960926     C                   ENDSR
160000960926     C*------------------------------------------------------------------------*
160100960926     C* CHKTIC - CONTROLLA IL TIPO INCASSO C/ASSEGNO
160200960926     C*------------------------------------------------------------------------*
160300960926     C     CHKTIC        BEGSR
160400960926     C*
160500960926     C     *LIKE         DEFINE    V1CTIC        WTIC
160600960926     C     *LIKE         DEFINE    §1AUDE        WUDE
160700010823     C     *LIKE         DEFINE    §1ACDL        WCDL
160800960926     C*
160900970704     C* RICHIESTA ELENCO
161000960926     C     '?'           SCAN      WTIC                                   99
161100960926IF  1C     *IN99         IFEQ      *ON
161200020919     C                   Z-ADD     1             §KUT
161300960926     C                   MOVEL     '1A'          §COD
161400960926     C                   MOVEL     *BLANKS       §KEY
161500960926     C                   MOVEL     *BLANKS       §DES
161600960926     C                   CALL      'X§TABER'
161700960926     C                   PARM                    §KUT              1 0
161800960926     C                   PARM                    §COD              2
161900960926     C                   PARM                    §KEY              8
162000960926     C                   PARM                    §DES             30
162100960926     C                   MOVEL     §KEY          WTIC
162200960926     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
162300960926E   1C                   ENDIF
162400960926     C*
162500010823     C* CONTROLLA ESISTENZA CODICE
162600020919     C                   Z-ADD     1             KTBKUT
162700960926     C                   MOVEL     '1A'          KTBCOD
162800960926     C                   MOVEL(P)  WTIC          KTBKEY
162900960926     C     KEYTAB        CHAIN     TABEL00F                           99
163000960926IF  1C     *IN99         IFEQ      *ON
163100960926     C     TBLFLG        OREQ      '*'
163200960926     C                   MOVEL     '1'           $ERR
163300960926X   1C                   ELSE
163400960926     C                   MOVEL     TBLUNI        DS1A
163500960926     C                   MOVEL     §1AUDE        WUDE                           *ABILITATO X DIV EST
163600010823     C                   MOVEL     §1ACDL        WCDL                           *FLAG DATA LIMITE CO
163700960926E   1C                   ENDIF
163800010823     C*
163900010823     C* CONTROLLO INSERIBILITA' TIPO INCASSO RISPETTO ALLA DIVISA
164000010823     C     $ERR          IFEQ      '0'
164100010823     C     WDVA          IFNE      'S'
164200010823     C     WUDE          ANDNE     'S'
164300010823     C                   MOVEL     '1'           $ERR
164400010823     C                   MOVEL     MSG(26)       $MSG
164500010823     C                   EXSR      CARMSG
164600010823     C                   SETON                                        7374
164700010823     C                   ELSE
164800010823     C     WCDL          IFEQ      'S'
164900010823     C     DSPV1C        IFGT      WDLC
165000010823     C                   MOVEL     '1'           $ERR
165100010823     C                   MOVEL     MSG(30)       $MSG
165200010823     C                   EXSR      CARMSG
165300010823     C                   SETON                                        7374
165400010823     C                   ENDIF
165500010823     C                   ENDIF
165600010823     C                   ENDIF
165700010823     C                   ENDIF
165800960926     C*
165900960926     C                   ENDSR
166000960614     C*------------------------------------------------------------------------*
166100960614     C* CHKKSC - CONTROLLA CODICE CLIENTE
166200960614     C*------------------------------------------------------------------------*
166300960614     C     CHKKSC        BEGSR
166400960614     C*
166500960614     C     *LIKE         DEFINE    V1CKSC        WKSC
166600960614     C     *LIKE         DEFINE    V1DKSC        WDKSC
166700960614     C*
166800970704     C* RICHIESTO ELENCO
166900960614     C     '?'           SCAN      WKSC                                   28
167000960614IF  1C     *IN28         IFEQ      *ON
167100960614     C                   CLEAR                   DSXC
167200020919     C                   Z-ADD     DUTKCI        DXCCAP                         *CAPOCONTO
167300110616     c                   eval      dxcaut = otaa1cabi
167400960614     C                   CALL      'XCLIR'
167500960614     C                   PARM                    DSXC
167600960614     C* SCOMPATTA
167700960614     C                   MOVEA     DXCKSC        KSA                            *DS INPUT -> SCH ALF
167800960614DO  2C     1             DO        30            J                 4 0
167900960614     C                   MOVE      KSA(J)        DSKSA                          *SCH ALF -> DS ALF
168000960614     C                   Z-ADD     DSKSN         KSC(J)                         *DS NUM -> SCH NUM
168100960614E   2C                   ENDDO
168200960614     C                   MOVEL     KSC(1)        WKSC
168300960614     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
168400960614E   1C                   ENDIF
168500960614     C*
168600970704     C* CONTROLLA ESISTENZA CODICE
168700960617IF  1C     WKSC          IFLE      *ZEROS                                       *SOLO VALORE NUMERIC
168800960614     C                   MOVEL     '1'           $ERR
168900960614X   1C                   ELSE
169000020919     C                   Z-ADD     1             KACKUT
169100020919     C                   Z-ADD     DUTKCI        KACKCC
169200960614     C                   MOVEL     WKSC          KACKSC
169300960614     C     KEYACO        CHAIN     CNACO00F                           99
169400960614IF  2C     *IN99         IFEQ      *ON                                          *CLIENTE INESISTENTE
169500960614     C     ACOFLG        ORNE      *BLANKS                                      *ANNULLATO
169600960614     C                   MOVEL     '1'           $ERR
169700960618X   2C                   ELSE                                                   *CLIENTE VALIDO
169800960618     C*
169900960618     C* CLIENTE DI FILIALE IN GESTIONE
170000020925     C                   MOVEL     WKSC          W003              3            *FIL+XXXX
170100020925IF  3C     W003          IFNE      '888'
170200100406     C**                 SETOFF                                           28
170300100406     C**   W003          LOOKUP    L1                                     28
170400100406IF  4C**   *IN28         IFEQ      *OFF
170500100406     C**                 MOVEL     '1'           $ERR
170600100406E   3C**                 ENDIF
170700100406     c                   clear                   tntaa1ds
170800100406     c                   eval      itaa1caut='§utegtc'
170900100406     c                   eval      itaa1ksc=%int(wksc)
171000100406     c                   clear                   kpjbu
171100100406     c                   movel     tntaa1ds      kpjbu
171200100406     c                   call      'TNTAA1C'
171300100406     c                   parm                    kpjba
171400100406     c                   movel     kpjbu         tntaa1ds
171500100406     c
171600100406    4c                   if        otaa1fabi='N'
171700100406     C                   MOVEL     '1'           $ERR
171800100406E   4C                   ENDIF
171900100406E   3C                   ENDIF
172000960618     C*
172100960614E   2C                   ENDIF
172200960614E   1C                   ENDIF
172300960614     C*
172400960614     C                   ENDSR
172500110623     C*------------------------------------------------------------------------*
172600110623     C* CHKINL - CONTROLLA L'INOLTRO
172700110623     C*------------------------------------------------------------------------*
172800110623     C     CHKINL        BEGSR
172900110623     C*
173000110623     C     *LIKE         DEFINE    V1cISO        WINL
173100110623     C     *LIKE         DEFINE    V1DISO        WDINL
173200110623     C*
173300110623     C* RICHIESTO ELENCO
173400110623     C                   SETOFF                                           28
173500110623     C     '?'           SCAN      WINL                                   28
173600110623IF  1C     *IN28         IFEQ      *ON
173700110623     C                   Z-ADD     1             §KUT              1 0
173800110623     C                   MOVEL     '9A'          §COD              2
173900110623     C                   MOVEL     *BLANKS       §KEY              8
174000110623     C                   MOVEL     *BLANKS       §DES             30
174100110623     C                   CALL      'X§TABER'
174200110623     C                   PARM                    §KUT
174300110623     C                   PARM                    §COD
174400110623     C                   PARM                    §KEY
174500110623     C                   PARM                    §DES
174600110623     C                   MOVEL     §KEY          WINL
174700110623     C                   MOVEL     §DES          WDINL
174800110623     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
174900110623E   1C                   ENDIF
175000110623     C*
175100110623     C* CONTROLLA ESISTENZA CODICE
175200110623     C                   MOVEL     1             KTBKUT
175300110623     C                   MOVEL     '9A'          KTBCOD
175400110623     C                   MOVEL(P)  WINL          KTBKEY
175500110623     C     KEYTAB        CHAIN     TABEL00F                           99
175600110623IF  1C     *IN99         IFEQ      *OFF
175700110623     C     TBLFLG        ANDEQ     *BLANKS
175800110623     C                   MOVEL     TBLUNI        DS9A
175900110623     C                   MOVEL     §9ADES        WDINL
176000110623X   1C                   ELSE
176100110623     C                   MOVEL     '1'           $ERR
176200110623E   1C                   ENDIF
176300110627     c*
176400110627     c                   if        $err='0' and §9autiop='N'
176500110627     C                   MOVEL     '1'           $ERR
176600110627E   1C                   ENDIF
176700110623     C*
176800110623     C                   ENDSR
176900960614     C*------------------------------------------------------------------------*
177000960614     C* CHKCTR - CONTROLLA TARIFFA CLIENTE
177100960614     C*------------------------------------------------------------------------*
177200960614     C     CHKCTR        BEGSR
177300960614     C*
177400960614     C     *LIKE         DEFINE    V1CCTR        WCTR
177500960617     C                   MOVEL     '0'           $CTR              1            *VALIDITA' TARIFFA
177600090429     C* RICERCA
177700090429     C     '?'           SCAN      v1cctr                                 30
177800090429     C*
177900090429    0C     *IN30         IFEQ      *ON
178000090429     C                   CLEAR                   PARAM4
178100090429     C                   MOVEL     v1cksc        PA4COD
178200090429     C                   Z-ADD     dutkci        PA4CCC
178300090429     c                   movel     '2'           pa4vpr
178400090429     c                   move      dspv1c        pa4dtr
178500090429     C                   MOVEL     PARAM4        KPJBU
178600090429     C                   CALL      'TNTA36R'
178700090429     C                   PARM                    KPJBA
178800090429     c* per riemettere videata
178900090429     C                   MOVEL     '1'           $ERR
179000090429     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
179100090429     C                   MOVEL     KPJBU         PARAM4
179200090429     c                   seton                                        71
179300090429     c
179400090429     C* SE TROVATO METTO QUELLO SCELTO
179500090429    1C     PA4CTR        IFNE      *BLANKS
179600090429     C                   MOVE      PA4CTR        v1cctr
179700090429     C                   MOVE      PA4dcv        v1dctr
179800090429   X1C                   ELSE
179900090429     c* Se non scelto lo abblanco
180000090429     C                   MOVEL     *blanks       v1cctr
180100090429     C                   MOVEL     *blanks       v1dctr
180200090429    1C                   END
180300090429     C*
180400090429   x0c                   else
180500960617     C*
180600970704     C* CONTROLLA ESISTENZA CODICE
180700960618IF  1C     WCTR          IFLT      *ZEROS                                       *SOLO VALORE NUMERIC
180800960617     C                   MOVEL     '1'           $ERR
180900090429     C                   MOVEL     MSG(21)       $MSG
181000090429     C                   EXSR      CARMSG
181100090429     C                   SETON                                        71
181200090429     c
181300960617X   1C                   ELSE
181400981214     C                   MOVEL     V1CKSC        KAMKSC
181500981214     C                   MOVEL     WCTR          KAMCTR
181600981214     C                   Z-ADD     999           KAMPRG
181700960617     C     KEYTAM        SETLL     TNTAM01L
181800960617     C     KE2TAM        READPE    TNTAM01L                               99
181900960617DO  2C     *IN99         DOWEQ     *OFF
182000960617IF  3C     TAMATB        IFEQ      *BLANKS
182100130108     c     tamddt        andle     dspv1c
182200130108     c
182300960703     C*---       TAMDST    ANDGEDSPV1C
182400021106     C*
182500021107     C* SE VUOTO IMPOSTO IL CAMPO A VIDEO RELATIVO AL DDT CON QUANTO PRESENTE SULLA TARIFFA
182600021107     C                   IF        V1CDDT = *BLANKS
182700021106     C                   MOVEL     TAMBAM        V1CDDT
182800021107     C                   ENDIF
182900960617     C                   MOVEL     '1'           $CTR
183000090429     c                   movel     tamdcv        v1dctr
183100960617     C                   SETON                                            99
183200960617E   3C                   ENDIF
183300960617     C  N99KE2TAM        READPE    TNTAM01L                               99
183400960617E   2C                   ENDDO
183500960617IF  2C     $CTR          IFEQ      '0'
183600960617     C                   MOVEL     '1'           $ERR
183700090429     C                   MOVEL     MSG(21)       $MSG
183800090429     C                   EXSR      CARMSG
183900090429     C                   SETON                                        71
184000090429   x2c                   else
184100090429     c
184200090429    3c                   if        (v1cksc<>savksc)  or (v1cctr<>savctr)
184300090429     c                   movel     v1cksc        savksc
184400090429     c                   movel     v1cctr        savctr
184500090429     c                   clear                   forzaBLO
184600090429     c                   clear                   forzaPA
184700090506     c                   clear                   forzaIE
184800090429    3c                   endif
184900090429     c
185000090429     c* Altrimenti controllo se tariffa corretta
185100090615    3c  n71              if        tambap='B'   and forzaBLO=' '
185200090429     C                   MOVEL     '1'           forzaBLO
185300090429     C                   MOVEL     '1'           $ERR
185400090429     C                   MOVEL     MSG(37)       $MSG
185500090429     C                   EXSR      CARMSG
185600090429     C                   SETON                                        71
185700090429    3c                   endif
185800090615    3c  n71              if        tambap='A' and v1ctpo='F' and forzaPA=' '
185900090429     C                   MOVEL     '1'           forzaPA
186000090429     C                   MOVEL     '1'           $ERR
186100090429     C                   MOVEL     MSG(39)       $MSG
186200090429     C                   EXSR      CARMSG
186300090429     C                   SETON                                        71
186400090429    3c                   endif
186500090615    3c  n71              if        tambap='P' and v1ctpo='A'  and forzaPA=' '
186600090429     C                   MOVEL     '1'           forzaPA
186700090429     C                   MOVEL     '1'           $ERR
186800090429     C                   MOVEL     MSG(38)       $MSG
186900090429     C                   EXSR      CARMSG
187000090429     C                   SETON                                        71
187100090429    3c                   endif
187200090506     c* Tariffa italia/estero
187300090506     c                   movel     tamflo        dsta01
187400090615     c  n71              if        §tadpd='S' and  v1cntw<>'DPD' and forzaIE=' '
187500120509     c                             and §ogntw<>'DPD'
187600090506     C                   MOVEL     '1'           forzaIE
187700090506     C                   MOVEL     '1'           $ERR
187800090506     C                   MOVEL     MSG(40)       $MSG
187900090506     C                   EXSR      CARMSG
188000090506     C                   SETON                                        71
188100090506    3c                   endif
188200090615     c  n71              if        §taFED='S' and  v1cntw<>'FED' and forzaIE=' '
188300090506     C                   MOVEL     '1'           forzaIE
188400090506     C                   MOVEL     '1'           $ERR
188500090506     C                   MOVEL     MSG(40)       $MSG
188600090506     C                   EXSR      CARMSG
188700090506     C                   SETON                                        71
188800090506    3c                   endif
188900090615     c  n71              if        §taFED=' ' and v1cntw<>'EEX' and forzaIE=' '
189000120509     c                             and tamfie='E' and §ogntw<>'EEX'
189100090506     C                   MOVEL     '1'           forzaIE
189200090506     C                   MOVEL     '1'           $ERR
189300090506     C                   MOVEL     MSG(40)       $MSG
189400090506     C                   EXSR      CARMSG
189500090506     C                   SETON                                        71
189600090506    3c                   endif
189700120509     c  n71              if        §tadpd=' '  and forzaIE=' '
189800120509     c                             and tamfie='I' and (v1cntw<>'   ' or
189900120509     c                              §ogntw='EEX' or §ogntw='FED' or
190000120509     c                              §ogntw='DPD')
190100090506     C                   MOVEL     '1'           forzaIE
190200090506     C                   MOVEL     '1'           $ERR
190300090506     C                   MOVEL     MSG(40)       $MSG
190400090506     C                   EXSR      CARMSG
190500090506     C                   SETON                                        71
190600090506    3c                   endif
190700090429     c
190800960617E   2C                   ENDIF
190900960617E   1C                   ENDIF
191000090429E   0C                   ENDIF
191100960614     C*
191200960614     C                   ENDSR
191300000000     C*------------------------------------------------------------------------*
191400000000     C* IMPVID  - IMPOSTAZIONI INIZIALI VIDEO
191500000000     C*------------------------------------------------------------------------*
191600000000     C     IMPVID        BEGSR
191700960528     C***
191800960528     C* SIMULAZIONE DELIVERY
191900960528     C***
192000000103     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
192100000103     C                   MOVE      N14           V1CDSP                         *DATA SPEDIZIONE
192200020919     C                   MOVEL     DUTPOU        V1CLNP                         *FILIALE PARTENZA
192300960704     C                   MOVEL     *BLANKS       V1CNAR                         *NAZIONE
192400960704     C                   MOVEL     'C'           V1CTSP                         *TIPO SERVIZIO
192500971126     C                   MOVEL     'F'           V1CTPO                         *TIPO PORTO
192600980116     C                   MOVEL     'N'           V1CFFD                         *FERMO DEPOSITO
192700000919     C                   MOVEL     'N'           V1CFDN                         *MERCE A MAGAZZINO
192800110817     C                   MOVEL     'N'           V1Cemd                         *email al destinat.
192900050413     c                   clear                   v1ctc1
193000050413     c                   clear                   v1ctc2
193100960528     C***
193200960528     C* SIMULAZIONE TASSAZIONE
193300960528     C***
193400960729     C                   Z-ADD     1             V1CNCL                         *COLLI
193500960528     C                   Z-ADD     1             V1CPKB                         *PESO
193600020313     C                   Z-ADD     0,005         V1CVLF                         *VOLUME
193700960703     C                   MOVEL     *ZEROS        V1CKSC                         *CODICE CLIENTE
193800960703     C                   MOVEL     *BLANKS       V1DKSC                         *DESCRIZIONE CLIENTE
193900960703     C                   MOVEL     *ZEROS        V1CCTR                         *CODICE TARIFFA
194000960926     C                   Z-ADD     *ZEROS        V1CCAS                         *IMPORTO C/ASSEGNO
194100960926     C                   MOVEL     *BLANKS       V1CVCA                         *VALUTA  C/ASSEGNO
194200960926     C                   MOVEL     *BLANKS       V1CTIC                         *INCASSO C/ASSEGNO
194300960926     C                   Z-ADD     *ZEROS        V1CIAS                         *IMPORTO ASSICURAZ.N
194400960926     C                   MOVEL     *BLANKS       V1CVAS                         *VALUTA  ASSICURAZ.N
194500020813     C                   Z-ADD     *ZEROS        V1CSCO                         *% DI SCONTO
194600050415     C                   Z-ADD     *ZEROS        V1Cqft                         *% DI SCONTO
194700090616     c                   clear                   forzaCAPuti
194800090616     c                   clear                   forzaIE
194900090616     c                   clear                   forzaBLO
195000090616     c                   clear                   forzaPA
195100120608     c                   clear                   savpkb
195200120608     c                   clear                   savtsp
195300160302     c                   clear                   savt_ntw
195400960704     C*
195500001121     C* RICHIAMO LA ROUTINE SEGUENTE PER GESTIRE LE CHIAMATE DALLA PROCEDURA "ORM"
195600001115     C                   EXSR      TSTPAR
195700001115     C*
195800000000     C                   ENDSR
195900001115     C*------------------------------------------------------------------------*
196000001115     C* TSTPAR - TEST DEI PARAMETRI IN INGRESSO PER VALORIZZAZIONE VIDEO DA CHIAMANTE
196100001115     C*------------------------------------------------------------------------*
196200001115     C     TSTPAR        BEGSR
196300001115     C*
196400001115     C* VERIFICA PARAMETRI
196500001116     C                   MOVEL     KPJBU         DSSI92
196600001116     C     D92OP0        IFEQ      'ORM'
196700001115     C     D92LNP        IFGT      *ZEROS
196800001115     C                   MOVEL     D92LNP        V1CLNP
196900001115     C                   ENDIF
197000001115     C     D92NAR        IFGT      *BLANKS
197100001115     C                   MOVEL     D92NAR        V1CNAR
197200001115     C                   ENDIF
197300001115     C     D92CAD        IFGT      *BLANKS
197400001115     C                   MOVEL     D92CAD        V1CCAD
197500001115     C                   ENDIF
197600001115     C     D92LOD        IFGT      *BLANKS
197700001115     C                   MOVEL     D92LOD        V1CLOD
197800001115     C                   ENDIF
197900001115     C     D92FFD        IFGT      *BLANKS
198000001115     C                   MOVEL     D92FFD        V1CFFD
198100001115     C                   ENDIF
198200001115     C     D92NCL        IFGT      *ZEROS
198300001115     C                   Z-ADD     D92NCL        V1CNCL
198400001115     C                   ENDIF
198500001115     C     D92PKB        IFGT      *ZEROS
198600001115     C                   Z-ADD     D92PKB        V1CPKB
198700001115     C                   ENDIF
198800001115     C     D92VLF        IFGT      *ZEROS
198900001115     C                   Z-ADD     D92VLF        V1CVLF
199000001115     C                   ENDIF
199100001115     C     D92KSC        IFGT      *ZEROS
199200001115     C                   MOVEL     D92KSC        V1CKSC
199300001115     C                   ENDIF
199400001115     C     D92CTR        IFGT      *ZEROS
199500001115     C                   MOVEL     D92CTR        V1CCTR
199600001115     C                   ENDIF
199700001115     C                   ENDIF
199800001115     C*
199900001115     C                   ENDSR
200000020225     C*------------------------------------------------------------------------*
200100020225     C* CARTAR - CARICA LE TARIFFE DI CARTELLO
200200020225     C*------------------------------------------------------------------------*
200300020225     C     CARTAR        BEGSR
200400020225     C*---
200500020225     C* TARIFFA DI CARTELLO
200600020225     C*---
200700160302     C                   CLEAR                   TRULc7DS
200800160302     C                   EVAL      Ic7TLA = 'L'
200900160302     C                   EVAL      Ic7tNTW = V1CNTW
201000160302     c                   eval      ic7pkg=v1cpkb
201100160302     C                   CALL      'TRULC7R'
201200160302     C                   PARM                    TRULc7DS
201300020225     C*
201400020225     C                   ENDSR
201500020925     C*--------------------------------------------------------------------------------------------*
201600020925     C* REPERISCE DATI UTENTE
201700020925     C*--------------------------------------------------------------------------------------------*
201800020925     C     REPDATIUTE    BEGSR
201900020925     C*
202000020925     C* REimposta le variabili di lavoro
202100020925     C                   CLEAR                   TIBS34DS
202200020925     C                   CLEAR                   AZUTEDS
202300020925     C                   CLEAR                   DDATIUTE
202400100406     C**                 CLEAR                   WABI              2
202500100406     C**                 CLEAR                   DLAT
202600020925     C*
202700020925     C* Reperisce i dati del profilo in ingresso
202800020925     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
202900020925     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
203000020925     C                   IN(E)     *DTAARA
203100020925     C                   IF        %Error                                       *NO errori
203200020925     C                   EVAL      I34TLA = 'L'
203300020925     C                   CALL      'TIBS34R'
203400020925     C                   PARM                    TIBS34DS
203500020925     C                   IN        *DTAARA
203600020925     C                   ENDIF
203700100406     c
203800100406     c* Richiamo solo per avere codice di abilitazione
203900100406     c                   clear                   tntaa1ds
204000100406     c                   eval      itaa1caut='§utegtc'
204100100406     c                   clear                   kpjbu
204200100406     c                   movel     tntaa1ds      kpjbu
204300100406     c                   call      'TNTAA1C'
204400100406     c                   parm                    kpjba
204500100406     c                   movel     kpjbu         tntaa1ds
204600100406     c
204700100406     c* Utente non abilitato alla gestione tariffe
204800100406     c                   if        otaa1fabi='N'
204900100406     C                   EVAL      $ERR  = '1'
205000100406     C                   EVAL      $FINE = '1'
205100100406     C                   ENDIF
205200020925     C*
205300020925     C* Se ci sono degli errori sull'abilitazione utente, esce
205400100406     C**                 SELECT
205500100406     C**                 WHEN      DUTERR = 'E'
205600100406     C**                 EVAL      $ERR  = '1'
205700100406     C**                 EVAL      $FINE = '1'
205800020925     C*
205900020925     C* NON siamo in sede (filiale remota o filiale locale)
206000020925     C* --> se 1° livello, abilitazioni al terminal
206100020925     C*     se 2° livello, abilitazioni al punto operativo
206200100406     C**                 WHEN      UTEAUT = *blanks
206300100406     C**                 IF        DUTLPO = '1'                                 *1° LIVELLO
206400100406     C**                 EVAL      WABI   = 'TP'
206500100406     C**                 ENDIF
206600100406     C**                 IF        DUTLPO = '2'                                 *2° LIVELLO
206700100406     C**                 EVAL      WABI   = 'PO'
206800100406     C**                 ENDIF
206900020925     C*
207000020925     C* siamo in sede
207100020925     C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
207200020925     C*     carica le abilitazioni proprie del profilo
207300100406     C**                 OTHER
207400100406     C**                 MOVEL     UTEFAF        DUTE01
207500100406     C**                 IF        §UTEIST <> *blanks                           *esiste specifica
207600100406     C**                 EVAL      WABI = §UTEIST
207700100406     C**                 ELSE                                                   *NON esiste specific
207800100406     C**                 EVAL      WABI = UTEAUT
207900100406     C**                 ENDIF
208000100406     C**                 ENDSL
208100020925     C*
208200020925     C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
208300100406     C**                 CLEAR                   TIBS02DS
208400100406     C*+                 MOVEL     'C'           T02MOD
208500100406     C**                 MOVEL     KNSIF         T02SIF
208600100406     C**                 MOVEL     'LAT'         T02COD
208700100406     C**                 MOVEL(P)  WABI          T02KE1
208800100406     C**                 EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
208900100406     C**                 CALL      'TIBS02R'
209000100406     C**                 PARM                    KPJBA
209100100406     C**                 PARM                    TIBS02DS
209200100406     c**
209300100406     C**                 EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
209400100406     C**                 IF        T02ERR = *blanks
209500100406     C**                 MOVEL     T02UNI        DLAT
209600100406     C**                 ENDIF
209700100406     C**                 IF        T02ERR <> *blanks OR
209800100406     C**                           §LATABI = 'S'
209900100406     C**                 EVAL      $ERR  = '1'
210000100406     C**                 EVAL      $FINE = '1'
210100100406     C**                 ENDIF
210200100406     C**
210300100406     C**Reperisce i P.O. gestibili dall'utente
210400100406     C**                 IF        $ERR <> '1'                                  *NO errori
210500100406     C**                 CLEAR                   TRUL31DS
210600100406     C**                 EVAL      I31ABI   = WABI
210700100406     C**                 EVAL      I31CDI   = DUTDIS
210800100406     C**                 EVAL      I31CAR   = DUTARE
210900100406     C**                 EVAL      I31CPO   = DUTPOU
211000100406     C**                 EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
211100100406     C**                 CALL      'TRUL31R'
211200100406     C**                 PARM                    KPJBA
211300100406     C**                 PARM                    TRUL31DS
211400100406     C**                 EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
211500100406if  2C**                 IF        O31POG > *zeros
211600100406     C**                 MOVEA     O31POG        L1
211700100406x   2C**                 ELSE
211800100406     C**                 EVAL      $ERR  = '1'
211900100406     C**                 EVAL      $FINE = '1'
212000100406e   2C**                 ENDIF
212100100406e   1C**                 ENDIF
212200020925     C*
212300020925     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
212400020925     C                   MOVEL(P)  RSUT          DSFIRS           20
212500020925     C*
212600020925     C                   ENDSR
212700000000     C*------------------------------------------------------------------------*
212800000000     C* *INZSR  - ROUTINE INIZIALE
212900000000     C*------------------------------------------------------------------------*
213000000000     C     *INZSR        BEGSR
213100000000     C*--------------------
213200000000     C* RICEVIMENTO PARAMETRI
213300000000     C*--------------------
213400000000     C     *ENTRY        PLIST
213500000000     C                   PARM                    KPJBA
213600000000     C*--------------------
213700000000     C* CAMPI RIFERITI AL DATA BASE
213800000000     C*--------------------
213900000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
214000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
214100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
214200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
214300960614     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNACO00F
214400960614     C     *LIKE         DEFINE    ACOKCC        KACKCC
214500960614     C     *LIKE         DEFINE    ACOKSC        KACKSC
214600981214     C     *LIKE         DEFINE    TAMKSC        KAMKSC                         *TNTAM00F
214700981214     C     *LIKE         DEFINE    TAMCTR        KAMCTR
214800981214     C     *LIKE         DEFINE    TAMPRG        KAMPRG
214900020925     C     *LIKE         DEFINE    KPJBU         DEPKPJBU                       *depositi
215000000000     C*--------------------
215100000000     C* CHIAVI DI LETTURA
215200000000     C*--------------------
215300000000     C* LETTURA AZORG01L - COMPLETA
215400000000     C     KEYORG        KLIST
215500000000     C                   KFLD                    KORFIL
215600000000     C* LETTURA TABEL00F - COMPLETA
215700000000     C     KEYTAB        KLIST
215800000000     C                   KFLD                    KTBKUT
215900000000     C                   KFLD                    KTBCOD
216000000000     C                   KFLD                    KTBKEY
216100960614     C* LETTURA CNACO00F - COMPLETA
216200960614     C     KEYACO        KLIST
216300960614     C                   KFLD                    KACKUT
216400960614     C                   KFLD                    KACKCC
216500960614     C                   KFLD                    KACKSC
216600960617     C* LETTURA TNTAM01L - COMPLETA
216700960617     C     KEYTAM        KLIST
216800981214     C                   KFLD                    KAMKSC
216900981214     C                   KFLD                    KAMCTR
217000981214     C                   KFLD                    KAMPRG
217100960617     C* LETTURA TNTAM01L - PARZIALE
217200960617     C     KE2TAM        KLIST
217300981214     C                   KFLD                    KAMKSC
217400981214     C                   KFLD                    KAMCTR
217500020225     C*
217600020225     C* DATA CORRENTE
217700020225     C                   TIME                    WTIME            14 0          ORA E DATA
217800020225     C                   MOVE      WTIME         DATEU             8 0          GG/MM/AAAA
217900020225     C     *EUR          MOVEL     DATEU         DATA_EUR
218000020225     C     *ISO          MOVEL     DATA_EUR      DATEU                          AAAA/MM/GG
218100120509     c
218200120509     c* carico tabella DPD
218300120509     C                   CLEAR                   DS3IDP
218400120509     C                   MOVEL     1             KTBKUT
218500120509     C                   MOVEL     '3I'          ktbcod
218600120509     C                   MOVEL(P)  'DPD'         ktbkey
218700120509     C     KEYTAB        CHAIN     TABEL00F
218800120509     c                   if        %found(tabel00f) and tblflg=*blanks
218900120509     C                   MOVEL     TBLUNI        DS3IDP
219000120509     c                   endif
219100960617     C*
219200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
219300000000     C                   EXSR      INZSTD
219400981214     C*
219500000000     C* IMPOSTAZIONI INIZIALI VIDEO
219600000000     C                   EXSR      IMPVID
219700000000     C*
219800000000     C* EMETTO FORMATO VIDEO
219900000000     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
220000000000     C*
220100000000     C                   ENDSR
220200000000     C*------------------------------------------------------------------------*
220300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
220400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
220500000000     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA'  A PARTICOLARI STANDARDS
220600000000     C* DI  PROGRAMMAZIONE  E  NELLA  MAGGIOR PARTE DEI  CASI NON E'  NECESSARIO
220700000000     C* MODIFICARLA, NEL CASO FARE ATTENZIONE.
220800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
220900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
221000000000     C*------------------------------------------------------------------------*
221100000000     C* INZSTD - ROUTINE INIZIALE STANDARD
221200000000     C*------------------------------------------------------------------------*
221300000000     C     INZSTD        BEGSR
221400000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
221500000000     C                   CALL      'XKEYS'
221600000000     C                   PARM                    DSKEYS
221700000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
221800000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
221900000000     C     XMSGPA        PLIST
222000000000     C                   PARM                    DSMSMS
222100000000     C                   PARM                    DSMSPG
222200000000     C                   PARM                    DSMSKE
222300000000     C                   PARM                    DSMSER
222400000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
222500970707     C                   MOVEL     'TISI92R'     DSMSPG                         *NOME PGM CHIAMANTE
222600970707     C                   MOVEL     'TISI92R'     VMCQUE                         *CODA MESSAGGI DESTI
222700000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
222800000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
222900000000     C* INIZIALIZZO FLAG CONTROLLO PGM
223000000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
223100000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
223200000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
223300000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
223400000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
223500000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
223600000000     C                   MOVEL     'G'           $TGES             1            *I=INTER. G=GESTIONE
223700000000     C* LANCIO ROUTINE PER VERIFICARE SE
223800000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
223900000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
224000020919     C                   EXSR      REPDATIUTE
224100000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
224200000000     C                   BITOFF    '01234567'    POSMI1            1
224300000000     C                   BITOFF    '01234567'    POSMI2            1
224400000000     C                   BITOFF    '01234567'    POSMA1            1
224500000000     C                   BITOFF    '01234567'    POSMA2            1
224600000000     C                   BITON     '34'          POSMI1            1            *18
224700000000     C                   BITON     '7'           POSMI2            1            *01
224800000000     C                   BITON     '34'          POSMA1            1            *18
224900000000     C                   BITON     '0'           POSMA2            1            *80
225000000000     C                   MOVE      POSMI2        POSMIN            2
225100000000     C                   MOVEL     POSMI1        POSMIN
225200000000     C                   MOVE      POSMA2        POSMAX            2
225300000000     C                   MOVEL     POSMA1        POSMAX
225400000000     C                   ENDSR
225500000000     C*------------------------------------------------------------------------*
225600000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
225700000000     C*------------------------------------------------------------------------*
225800000000     C     FUNSTD        BEGSR
225900000000     C                   SELECT
226000000000     C     KEY           WHENEQ    F3
226100000000     C                   EXSR      F03STD
226200000000     C     KEY           WHENEQ    F12
226300000000     C                   EXSR      F12STD
226400000000     C     KEY           WHENEQ    F24
226500000000     C                   EXSR      F24STD
226600000000     C                   OTHER
226700000000     C                   MOVEL     MSG(2)        $MSG
226800000000     C                   EXSR      CARMSG
226900000000     C                   ENDSL
227000000000     C                   ENDSR
227100000000     C*------------------------------------------------------------------------*
227200000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
227300000000     C*------------------------------------------------------------------------*
227400000000     C     F03STD        BEGSR
227500000000     C*
227600000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
227700000000     C                   MOVEL     '1'           $F03
227800000000     C*
227900000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
228000000000     C                   MOVEL     '1'           $FINE
228100000000     C*
228200000000     C                   ENDSR
228300000000     C*------------------------------------------------------------------------*
228400000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
228500000000     C*------------------------------------------------------------------------*
228600000000     C     F12STD        BEGSR
228700000000     C*
228800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
228900000000     C                   MOVEL     '1'           $FINE
229000000000     C*
229100000000     C                   ENDSR
229200000000     C*------------------------------------------------------------------------*
229300000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
229400000000     C*------------------------------------------------------------------------*
229500000000     C     F24STD        BEGSR
229600000000     C*
229700000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
229800000000     C                   EXSR      OUTFUN
229900000000     C*
230000000000     C                   ENDSR
230100000000     C*------------------------------------------------------------------------*
230200000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
230300000000     C*          CODA MESSAGGI DEL PGM
230400000000     C*------------------------------------------------------------------------*
230500000000     C     CARMSG        BEGSR
230600000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
230700000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
230800000000     C     MSGULT        IFEQ      0
230900000000     C                   MOVEL     '1'           *IN40
231000970707     C                   WRITE     SI92DVK
231100000000     C                   MOVEL     '0'           *IN40
231200000000     C                   ENDIF
231300000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
231400000000     C                   MOVEL     $MSG          DSMSMS
231500000000     C                   CALL      'XMSG'        XMSGPA
231600000000     C     DSMSER        IFEQ      0
231700000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
231800000000     C                   ENDIF
231900000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
232000000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
232100970707     C                   WRITE     SI92DVM                                      *SFL MESS
232200000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
232300000000     C                   MOVEL     *BLANKS       $MSG
232400000000     C                   ENDSR
232500000000     C*------------------------------------------------------------------------*
232600000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
232700000000     C*------------------------------------------------------------------------*
232800000000     C     OUTMSG        BEGSR
232900000000     C* VISUALIZZAZIONE MESSAGGIO
233000000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
233100970707     C                   WRITE     SI92DVK                                      *CONTR MESS
233200000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
233300000000     C                   Z-ADD     *ZEROS        MSGULT
233400000000     C                   ENDSR
233500000000     C*------------------------------------------------------------------------*
233600000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
233700000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
233800000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
233900000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
234000000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
234100000000     C*------------------------------------------------------------------------*
234200000000     C     CARFUN        BEGSR
234300000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
234400000000     C     1             DO        31            I                 5 0          --- 1 -->
234500000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
234600000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
234700000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
234800020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
234900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
235000000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
235100000000     C                   ELSE                                                   *INTERROG.
235200000000     C                   MOVEL     FUNASI        FAT(I)
235300000000     C                   ENDIF
235400000000     C                   ELSE                                                   <-- 2 --> *FILIALE
235500000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
235600000000     C                   MOVEL     FUNAFG        FAT(I)
235700000000     C                   ELSE                                                   *INTERROG.
235800000000     C                   MOVEL     FUNAFI        FAT(I)
235900000000     C                   ENDIF
236000000000     C                   ENDIF                                                  <-- 2 ---
236100000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
236200000000     C                   MOVEL     USER          WEDP              3            *PROFILO
236300000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
236400000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
236500000000     C                   ELSE
236600000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
236700000000     C                   ENDIF
236800000000     C                   ENDIF
236900000000     C                   ENDDO                                                  <-- 1 ---
237000000000     C                   ENDSR
237100000000     C*------------------------------------------------------------------------*
237200000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
237300000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
237400000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
237500000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
237600000000     C*------------------------------------------------------------------------*
237700000000     C     OUTFUN        BEGSR
237800000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
237900000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
238000000000     C                   ENDDO
238100000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
238200000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
238300000000     C     1             DO        31            I
238400000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
238500000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
238600000000     C                   ADD       1             FUNNAT
238700000000     C                   ENDIF
238800000000     C                   ENDDO
238900000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
239000000000     C     FUNNAT        IFGT      8
239100000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
239200000000     C                   ENDIF
239300000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
239400000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
239500000000     C                   Z-ADD     8             FUNVIS            5 0
239600000000     C     FAT(23)       IFEQ      'S'
239700000000     C                   SUB       1             FUNVIS
239800000000     C                   ENDIF
239900000000     C     FAT(24)       IFEQ      'S'
240000000000     C                   SUB       1             FUNVIS
240100000000     C                   ENDIF
240200000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
240300000000     C                   Z-ADD     *ZEROS        W                 5 0
240400000000     C                   ADD       1             FUNULT
240500000000     C     FUNULT        IFGT      31
240600000000     C                   Z-ADD     1             FUNULT
240700000000     C                   ENDIF
240800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
240900000000     C     FUNULT        DO        31            I                              --- 1 -->
241000000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
241100000000     C     W             IFLT      FUNVIS                                       --- 2 -->
241200000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
241300000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
241400000000     C     FDE(I)        ANDNE     *BLANKS
241500000000     C     I             IFEQ      23                                           *TOLGO F23
241600000000     C     I             OREQ      24                                           *E F24
241700000000     C                   ELSE
241800000000     C                   ADD       1             W
241900000000     C                   MOVEL     FDE(I)        FUV(W)
242000000000     C                   ENDIF
242100000000     C                   ENDIF                                                  <-- 3 ---
242200000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
242300000000     C                   Z-ADD     I             FUNULT
242400000000     C                   ENDIF                                                  <-- 2 ---
242500000000     C                   ENDDO                                                  <-- 1 ---
242600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
242700000000     C                   MOVEL     FUV(1)        VFD001
242800000000     C                   MOVEL     FUV(2)        VFD002
242900000000     C                   MOVEL     FUV(3)        VFD003
243000000000     C                   MOVEL     FUV(4)        VFD004
243100000000     C                   MOVEL     FUV(5)        VFD005
243200000000     C                   MOVEL     FUV(6)        VFD006
243300000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
243400000000     C     FAT(24)       ANDEQ     'N'
243500000000     C                   MOVEL     FUV(7)        VFD007
243600000000     C                   MOVEL     FUV(8)        VFD008
243700000000     C                   ELSE                                                   <-- 1 -->
243800000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
243900000000     C     FAT(24)       ANDEQ     'S'
244000000000     C                   MOVEL     FDE(23)       VFD007
244100000000     C                   MOVEL     FDE(24)       VFD008
244200000000     C                   ELSE                                                   <-- 2 -->
244300000000     C                   MOVEL     FUV(7)        VFD007
244400000000     C     FAT(23)       IFEQ      'S'
244500000000     C                   MOVEL     FDE(23)       VFD008
244600000000     C                   ELSE
244700000000     C                   MOVEL     FDE(24)       VFD008
244800000000     C                   ENDIF
244900000000     C                   ENDIF                                                  <-- 2 ---
245000000000     C                   ENDIF                                                  <-- 1 ---
245100000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
245200000000     C                   ENDSR
245300000000     C*------------------------------------------------------------------------*
245400000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
245500000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
245600000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
245700000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
245800000000     C*------------------------------------------------------------------------*
245900000000     C     CONFUN        BEGSR
246000000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
246100000000     C     1             DO        31            I                              --- 1 -->
246200000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
246300000000     C* CONTROLLO SE IL TASTO E' ATTIVO
246400000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
246500000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
246600000000     C                   EXSR      CARMSG
246700000000     C                   MOVEL     '1'           $ERR
246800000000     C                   ENDIF                                                  <-- 3 ---
246900000000     C                   ENDIF                                                  <-- 2 ---
247000000000     C                   ENDDO                                                  <-- 1 ---
247100000000     C                   ENDSR
247200000000     C*------------------------------------------------------------------------*
247300000000** -MSG- MESSAGGI
247400970707TISI92R- Tasto funzionale non valido.                                          1
247500970707TISI92R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
247600970707TISI92R- Opzione non valida.                                                   3
247700970707TISI92R- Opzione abilitata ma non gestita. Avvertire CED.                      4
247800970707TISI92R- Inizio elenco.                                                        5
247900970707TISI92R- Fine elenco.                                                          6
248000970707TISI92R- Data errata                                                           7
248100970707TISI92R- Punto operativo errato                                                8
248200970707TISI92R- Tipo servizio errato                                                  9
248300970707TISI92R- Nazione errata                                                       10
248400090615TISI92R- CAP errato per la nazione indicata                                   11
248500110627TISI92R- Tipo inoltro forzato errato o non utilizzabile                       12
248600971126TISI92R- Tipo porto errato                                                    13
248700970714TISI92R- Località inesistente in questo cap: immettere '?' per la scelta      14
248800980116TISI92R- Immettere uno dei valori possibili                                   15
248900020225TISI92R- Impossibile specificare network per nazione Italia.                  16
249000970707TISI92R- Immettere il numero dei colli                                        17
249100970707TISI92R- Immettere il peso   (in Kg)                                          18
249200970707TISI92R- Immettere il volume (in Mc)                                          19
249300970707TISI92R- Codice cliente errato o non in gestione                              20
249400970707TISI92R- Codice tariffa errato                                                21
249500010823TISI92R- Divisa errata                                                        22
249600970707TISI92R- Tipo incasso C/Assegno errato                                        23
249700970707TISI92R- C/Assegno obbligatorio se immesso la valuta o il tipo incasso        24
249800970707TISI92R- Non sono ammessi decimali per la valuta indicata                     25
249900010820TISI92R- Tipo incasso non utilizzabile per la divisa indicata                 26
250000970707TISI92R- Importo assicurazione obbligatorio se immessa la valuta              27
250100980703TISI92R- Lasciare vuoti i dati del mittente se porto franco                   28
250200980703TISI92R- Immettere i dati del mittente se porto assegnato                     29
250300010823TISI92R- Divisa non più accettabile                                           30
250400020226TISI92R- Network indicato inaccettabile per estero                            31
250500020514TISI92R- Nazione e Network incompatibili.                                     32
250600030320ATTENZIONE!! Volume troppo alto. Ricalcolare.                                 33
250700050413Consegna particolare errata                                                   34
250800050413Non è possibile impostare la stessa consegna particolare 2 volte              35
250900050428Se FERMO DEPOSITO, Inoltro non forzabile                                      36
251000090429E' stata immessa una tariffa BLOCCATA. Enter forza                            37
251100090429Tariffa valida solo in Partenza non corretta per porti Assegnati.enter forza  38
251200090429Tariffa valida solo in Arrivo  non corretta per porti franco.enter forza      39
251300090615Tariffa incongruente col Network selezionato. Enter forza                     40
251400090617Network non utilizzabile per tutte le spedizioni:ENTER forza                  41
251500090617Network non utilizzabile per spedizioni con C/Assegno:ENTER forza             42
251600090617Network non utilizzabile per spedizioni in P.Assegnato:ENTER forza            43
251700090617Network non utilizzabile per spedizioni in P.Ass.o C/ASS:ENTER forza          44
251800090617Assegnato export per il network solo con ORM reale commissionato.ENTR forza   45
251900110923Superati i limiti per tipo servizio. Enter Forza                              46
252000120509Spedizione D.P.D. : deve essere di 1 collo                                    47
252100120509Spedizione D.P.D. : non ammesso un peso superiore a                           48
252200000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
252300000000F1    -                   -    NN     NN
252400000000F2    -                   -    NN     NN
252500000000F3    -F3=Fine            -    SS     SS
252600000000F4    -                   -    NN     NN
252700000000F5    -                   -    NN     NN
252800000000F6    -F6=Lancio          -    SN     SN
252900000000F7    -                   -    NN     NN
253000000000F8    -                   -    NN     NN
253100000000F9    -                   -    NN     NN
253200000000F10   -                   -    NN     NN
253300030320F11   -F11=Volume         -    SN     SN
253400000000F12   -F12=Ritorno        -    SS     SS
253500000000F13   -                   -    NN     NN
253600000000F14   -                   -    NN     NN
253700000000F15   -                   -    NN     NN
253800000000F16   -                   -    NN     NN
253900000000F17   -                   -    NN     NN
254000000000F18   -                   -    NN     NN
254100000000F19   -                   -    NN     NN
254200000000F20   -                   -    NN     NN
254300000000F21   -                   -    NN     NN
254400000000F22   -                   -    NN     NN
254500000000F23   -                   -    NN     NN
254600000000F24   -F24=Altri tasti    -    NN     NN
254700000000ENTER -                   -    SS     SS
254800000000ROLLUP-                   -    NN     NN
254900000000ROLLDN-                   -    NN     NN
255000000000HELP  -                   -    NN     NN
255100000000PRINT -                   -    NN     NN
255200000000BSPACE-                   -    NN     NN
255300000000CLEAR -                   -    NN     NN
