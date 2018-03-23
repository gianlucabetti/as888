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
002000170120     D MSG             S             78    DIM(49) CTDATA PERRCD(1)             *MESSAGGI
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
008100170120     d
008200170120     D trul22ds      E DS
008300170120     D fnlv55ds      E DS
008400000000     D*------------------
008500000000     D* DS CONTROLLO DATE (8)
008600000000     D*------------------
008700000000     D WLBDA8          DS                  INZ
008800000000     D  G08DAT                 1      8  0
008900000000     D  G08INV                 9     16  0
009000000000     D  G08ERR                17     17
009100000000     D  G08TGI                18     22  0
009200000000     D*------------------
009300000000     D* DS DATA (8)
009400000000     D*------------------
009500000000     D                 DS
009600000000     D  GIOR                   1      2  0
009700000000     D  MESE                   3      4  0
009800000000     D  ANNO                   5      8  0
009900981214     D  DATA                   1      8  0
010000090506     d
010100090506     D DSta01        E DS
010200120509     D og143         E DS
010300120509
010400120509     D ds3idp        E DS
010500000000     D*------------------
010600000000     D* DS NAZIONI
010700000000     D*------------------
010800000000     D DS15          E DS
010900050413     D*------------------
011000050413     D* DS consegne particolari
011100050413     D*------------------
011200050413     D DS1P          E DS
011300000000     D*------------------
011400000000     D* DS TIPI SERVIZIO
011500000000     D DS5E          E DS
011600110623     D*------------------
011700110623     D* DS TIPI inoltro
011800110623     D DS9A          E DS
011900970704     D*-------------------
012000970704     D* DS VALUTE
012100970704     D*-------------------
012200970704     D DSCV          E DS
012300010823     D DSCW          E DS
012400970704     D*-------------------
012500970704     D* DS TIPI INCASSO C/ASSEGNO
012600970704     D*-------------------
012700970704     D DS1A          E DS
012800970704     D*------------------
012900970704     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
013000970704     D*------------------
013100970704     D DSXC          E DS                  EXTNAME(XCLIRDS)
013200970704     D*---
013300970704     D                 DS
013400970704     D  DSKSN                  1      4P 0
013500970704     D  DSKSA                  1      4
013600970704     D*------------------
013700970704     D* DS "TISI00R" - INTERROGAZIONE ARCHIVI CAPPARIO
013800970704     D*------------------
013900970704     D DSSI00        E DS                  EXTNAME(TISI00DS)
014000000000     D*------------------
014100970704     D* DS "TISI95R" CONTROLLA CAP
014200000000     D*------------------
014300970704     D DSSI95        E DS                  EXTNAME(TISI95DS)
014400090615     D fnlv13ds      E DS
014500090615     d
014600090615     D tisi97ds      E DS
014700090616     D dcppflo       E DS
014800970704     D*-------------------
014900970704     D* DS DI PROCEDURA
015000970704     D*-------------------
015100970707     D DSSI92        E DS                  EXTNAME(TISI92DS) INZ
015200970704     D*-------------------
015300970704     D* ARCHITETTURA
015400970704     D*-------------------
015500970704     D KPJBA         E DS
015600020225     D*-------------------
015700020225     D* VARIABILI DI WRK
015800020225     D*-------------------
015900020225     D DATA_EUR        S               D   DATFMT(*eur)
016000020925     D*------------
016100020925     D* DS REPERIMENTO DATI UTENTE
016200020925     D*------------
016300020925     D TIBS34DS      E DS                                                       *Profili utente
016400020925     D DDATIUTE      E DS                                                       *Dati utente
016500020925     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
016600100406     d* Controllo abilitazioni utente
016700100406     d tntaa1ds      e ds
016800020925     D*------------
016900020925     D* DS CONTROLLO/SCELTA TABELLE
017000020925     D*------------
017100020925     D TIBS02DS      E DS                                                       *Ricerca tabella
017200090429     D*-------------------
017300090429     D* PASSO IN SCELTA COD TARIFFA DI TARIFFA/OFFERTA - TNTA36R -
017400090429     D PARAM4          DS
017500090429     D  PA4COD                 6     12  0
017600090429     D  PA4CTR                13     15
017700090429     D  PA4FLG                16     16
017800090429     D  PA4VPR                17     17
017900090429     D  PA4CCC                18     21  0
018000090429     D  PA4DCV                22     36
018100090429     d  pa4prg                37     39
018200090429     d  pa4dtr                40     47
018300020925     D*------------------
018400020925     D* SCHIERA X LINEEE CONSENTITE
018500020925     D*------------------
018600020925     D L1              S              3    DIM(250)
018700030320
018800030403     d wflg            s              1    Inz
018900090429     d forzaPA         s              1    Inz
019000090429     d forzaBLO        s              1    Inz
019100090506     d forzaIE         s              1    Inz
019200090616     d forzaCapUti     s              1    Inz
019300040301     d p26vol          s              8  3
019400050413     d wtcx            s                   like(v1ctc1)
019500050413     d wdtcx           s                   like(v1dtc1)
019600090618     d svtbluni        s                   like(tbluni)
019700090616     d savksc          s                   like(v1cksc) inz
019800090616     d savctr          s                   like(v1cctr) inz
019900090616     d savtpo          s                   like(v1ctpo) inz
020000090616     d savntw          s                   like(v1cntw) inz
020100090616     d savcas          s                   like(v1ccas) inz
020200090618     d savcad          s                   like(v1ccad) inz
020300090618     d savnar          s                   like(v1cnar) inz
020400110923     d wncl_pre        s                   like(v1cncl) inz
020500110923     d wpes_pre        s                   like(v1cpkb) inz
020600110923     d wvol_pre        s                   like(v1cvlf) inz
020700110923     d savtsp          s                   like(v1ctsp) inz
020800120608     d savpkb          s                   like(v1cpkb) inz
020900160302     d savt_ntw        s                   like(ic7tntw) inz
021000030320
021100000000     C*------------------------------------------------------------------------*
021200000000     C* MAIN LINE
021300000000     C*------------------------------------------------------------------------*
021400000000     C*
021500000000     C* CICLO FINO A CHE:
021600000000DO  1C     $FINE         DOUEQ     '1'
021700000000     C*
021800000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
021900000000     C*
022000000000     C* CONTROLLA GESTIONE VIDEO
022100000000IF  2C     $GES          IFEQ      'V1'
022200000000     C                   EXSR      GESV1                                        *PARAMETRI
022300000000E   2C                   ENDIF
022400000000     C*
022500000000E   1C                   ENDDO
022600000000     C* FINE PGM
022700960729     C                   EXSR      CHIUDI                                       *CHIUDE FILE
022800000000     C                   SETON                                        LR
022900000000     C*------------------------------------------------------------------------*
023000000000     C* CHIUDI  - CHIUDE EVENTUALI PGM CON USCITA RETURN
023100000000     C*------------------------------------------------------------------------*
023200000000     C     CHIUDI        BEGSR
023300970704     C* TISI95R
023400970704     C                   CLEAR                   DSSI95
023500970704     C                   MOVEL     'C'           I95TLA                         'C'=CHIUDI
023600970704     C                   CALL      'TISI95R'                            99
023700970704     C                   PARM                    DSSI95
023800100406     C*
023900100406     C                   clear                   tntaa1ds
024000100406     C                   movel     'C'           Itaa1Tla
024100100406     C                   movel(p)  tntaa1ds      kpjbu
024200100406     C                   CALL      'TNTAA1C'
024300100406     C                   PARM                    KPJBA
024400110922     c*
024500110922     C                   clear                   trul73ds
024600110922     C                   movel     'C'           I73tla
024700110922     C                   CALL      'TRUL73R'
024800110922     C                   PARM                    trul73ds
024900100406     c
025000000000     C*
025100000000     C                   ENDSR
025200000000     C*------------------------------------------------------------------------*
025300000000     C* GESV1 - GESTIONE VIDEO
025400000000     C*------------------------------------------------------------------------*
025500000000     C     GESV1         BEGSR
025600000000     C*
025700000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
025800000000     C                   EXSR      CARV1                                        *CARICA VIDEO
025900000000E   1C                   ENDIF
026000000000     C*
026100000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
026200000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
026300000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
026400000000E   1C                   ENDIF
026500000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
026600000000     C*
026700970707     C                   EXFMT     SI92DV1                                      *PARAMETRI
026800000000     C*
026900000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
027000000000     C*
027100000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
027200000000SL  2C                   SELECT
027300000000     C     KEY           WHENEQ    ENTER
027400000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
027500000000     C     KEY           WHENEQ    F6
027600000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
027700030320     C     KEY           WHENEQ    F11
027800040301     C                   EXSR      F11GES                                       *GEST.F11(Volume)
027900000000     C                   OTHER
028000000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
028100000000E   2C                   ENDSL
028200000000E   1C                   ENDIF
028300000000     C*
028400000000     C                   ENDSR
028500000000     C*------------------------------------------------------------------------*
028600000000     C* CARV1 - CARICA VIDEO
028700000000     C*------------------------------------------------------------------------*
028800000000     C     CARV1         BEGSR
028900000000     C*
029000000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
029100000000     C*
029200000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
029300000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
029400000000     C*
029500970707     C                   WRITE     SI92DVT                                      *TESTATA
029600970707     C                   WRITE     SI92DVF                                      *FUNZIONI
029700000000     C*
029800000000     C                   ENDSR
029900000000     C*------------------------------------------------------------------------*
030000000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO
030100000000     C*------------------------------------------------------------------------*
030200000000     C     ENTV1         BEGSR
030300000000     C*
030400000000     C* CONTROLLA I CAMPI IMMESSI
030500000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
030600000000     C*
030700000000     C                   ENDSR
030800000000     C*------------------------------------------------------------------------*
030900000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
031000000000     C*------------------------------------------------------------------------*
031100000000     C     F06GES        BEGSR
031200000000     C*
031300000000     C* CONTROLLA I CAMPI IMMESSI
031400000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
031500000000     C*
031600000000     C* NON CI SONO ERRORI
031700000000IF  1C     $ERR          IFEQ      '0'
031800000000     C*
031900000000     C* LANCIA PROGRAMMA
032000001115     C                   CLEAR                   DSSI92
032100000000     C                   MOVEL     '0'           D92F03
032200000000     C                   MOVEL     '0'           D92F12
032300000000     C                   MOVEL     '0'           D92ERR
032400000000     C                   MOVEL     *BLANKS       D92MSG
032500000000     C                   Z-ADD     DSPV1C        D92DSP
032600000000     C                   MOVEL     V1CLNP        D92LNP
032700000000     C                   MOVEL     V1CNAR        D92NAR
032800000000     C                   MOVEL     V1CCAD        D92CAD
032900970704     C                   MOVEL     V1CLOD        D92LOD
033000000000     C                   MOVEL     V1CTSP        D92TSP
033100000000     C                   MOVEL     V1CISO        D92ISO
033200980116     C                   MOVEL     V1CFFD        D92FFD
033300000919     C                   MOVEL     V1CFDN        D92FDN
033400980703     C                   MOVEL     V1CTPO        D92TPO
033500980703     C                   MOVEL     V1CNZM        D92NZM
033600980703     C                   MOVEL     V1CCAM        D92CAM
033700980703     C                   MOVEL     V1CLOM        D92LOM
033800960528     C                   Z-ADD     V1CNCL        D92NCL
033900960528     C                   Z-ADD     V1CPKB        D92PKB
034000960528     C                   Z-ADD     V1CVLF        D92VLF
034100960617     C                   MOVEL     V1CKSC        D92KSC
034200960614     C                   MOVEL     V1CCTR        D92CTR
034300010822     C                   Z-ADD     V1CCAS        D92CAS
034400960926     C                   MOVEL     V1CVCA        D92VCA
034500960926     C                   MOVEL     V1CTIC        D92TIC
034600010822     C                   Z-ADD     V1CIAS        D92IAS
034700960926     C                   MOVEL     V1CVAS        D92VAS
034800050415     C                   Z-ADD     V1Cqft        D92qft
034900020813     C                   Z-ADD     V1CSCO        D92SCO
035000021106     C                   MOVEL     V1CDDT        D92DDT
035100020225     C                   MOVEL     V1CNTW        D92NTW
035200050413     c                   if        v1ctc1=' ' and v1ctc2<>' '
035300050413     c                   movel     v1ctc2        d92tc1
035400050413     c                   else
035500050413     C                   MOVEL     V1Ctc1        D92tc1
035600050413     C                   MOVEL     V1Ctc2        D92tc2
035700050413     c                   endif
035800110818     c                   movel     v1cemd        d92emd
035900050413     c
036000020514     C                   MOVEL     DSSI92        KPJBU
036100970707     C                   CALL      'TISI93R'
036200000000     C                   PARM                    KPJBA
036300970707     C                   MOVEL     KPJBU         DSSI92
036400970704     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
036500000000     C*
036600000000     C* RITORNA MESSAGGIO DI ERRORE, LO VISUALIZZA
036700000000IF  2C     D92ERR        IFNE      '0'                                          *ERRORE
036800000000     C                   MOVEL     D92ERR        $ERR
036900000000     C                   MOVEL     D92MSG        $MSG
037000000000     C                   EXSR      CARMSG
037100000000X   2C                   ELSE                                                   *NO ERRORE
037200000000     C*
037300000000     C* SE RITORNA PREMUTO F3 ESCE DAL PROGRAMMA, SE F12 NO
037400000000IF  3C     D92F03        IFEQ      '1'
037500000000     C     D92F12        OREQ      '1'
037600000000     C                   MOVEL     '1'           $ERR                           *NON VA AVANTI
037700000000IF  4C     D92F03        IFEQ      '1'
037800000000     C                   MOVEL     '1'           $F03                           *FINE PGM
037900000000     C                   MOVEL     '1'           $FINE                          *FINE CICLO
038000000000E   4C                   ENDIF
038100000000E   3C                   ENDIF
038200000000E   2C                   ENDIF
038300000000     C*
038400000000E   1C                   ENDIF
038500030403
038600030403     c                   Clear                   wflg
038700000000     C*
038800000000     C                   ENDSR
038900030320     C*------------------------------------------------------------------------*
039000030320     C* F11GES - GEST. TASTO FUNZ. F11 ==> Calcolo volume
039100030320     C*------------------------------------------------------------------------*
039200030320     C     F11GES        BEGSR
039300030320
039400090429     c                   If        wflg = *Blanks
039500030403     c                   Clear                   v1cvlf
039600030403     c                   Eval      wflg = '1'
039700030403     c                   EndIf
039800040301
039900040301     c                   Clear                   p26vol
040000040301     c                   Call      'TRUL36R'
040100040301     c                   Parm                    p26vol
040200030403
040300040301     C                   Add       p26vol        v1cvlf
040400040301     c                   Eval      $tesvd = '1'                                 *RIEMETTERE TESTATA
040500040301
040600030320     C*
040700030320     C                   ENDSR
040800000000     C*------------------------------------------------------------------------*
040900000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
041000000000     C*------------------------------------------------------------------------*
041100000000     C     DECODI        BEGSR
041200000000     C*
041300960528     C                   SETOFF                                       606162          *DELIVERY
041400000000     C                   SETOFF                                       636465
041500980116     C                   SETOFF                                       667778
041600960528     C                   SETOFF                                       676869          *TASSAZIONE
041700960925     C                   SETOFF                                       707172
041800960925     C                   SETOFF                                       737475
041900980703     C                   SETOFF                                       767980
042000020225     C                   SETOFF                                       818283
042100050413     C                   SETOFF                                       848586
042200110817     C                   SETOFF                                       8788
042300960528     C***
042400960528     C* SIMULAZIONE DELIVERY
042500960528     C***
042600000000     C*--------------------
042700000000     C* DATA SPEDIZIONE
042800000000     C*--------------------
042900000000     C     *LIKE         DEFINE    V1CDSP        DSPV1C
043000000000IF  1C     $ERR          IFEQ      '0'
043100000000     C                   Z-ADD     V1CDSP        WDA8
043200000000     C                   Z-ADD     *ZEROS        DA8W
043300000000     C                   EXSR      CHKDA8
043400000000     C                   Z-ADD     DA8W          DSPV1C
043500000000     C                   Z-ADD     WDA8          V1CDSP
043600000000IF  2C     $ERR          IFEQ      '1'
043700000000     C                   MOVEL     MSG(7)        $MSG
043800000000     C                   EXSR      CARMSG
043900000000     C                   SETON                                        60
044000000000E   2C                   ENDIF
044100000000E   1C                   ENDIF
044200000000     C*--------------------
044300000000     C* LINEA DI PARTENZA
044400000000     C*--------------------
044500000000IF  1C     $ERR          IFEQ      '0'
044600000000     C                   MOVEL     V1CLNP        WLNP
044700970704     C                   MOVEL     *BLANKS       WDLNP
044800000000     C                   EXSR      CHKLNP
044900000000     C                   MOVEL     WLNP          V1CLNP
045000000000     C                   MOVEL     WDLNP         V1DLNP
045100960704IF  2C     $ERR          IFEQ      '1'
045200000000     C                   MOVEL     MSG(8)        $MSG
045300000000     C                   EXSR      CARMSG
045400000000     C                   SETON                                        61
045500000000E   2C                   ENDIF
045600000000E   1C                   ENDIF
045700000000     C*--------------------
045800000000     C* NAZIONE
045900000000     C*--------------------
046000000000IF  1C     $ERR          IFEQ      '0'
046100970704     C                   MOVEL     V1CNAR        WNAR
046200970704     C                   MOVEL     *BLANKS       WDNAR
046300000000     C                   EXSR      CHKNAR
046400000000     C                   MOVEL     WNAR          V1CNAR
046500000000     C                   MOVEL     WDNAR         V1DNAR
046600000000IF  2C     $ERR          IFEQ      '1'
046700000000     C                   MOVEL     MSG(10)       $MSG
046800000000     C                   EXSR      CARMSG
046900080912     c                   clear                   svtbluni
047000000000     C                   SETON                                        62
047100080912     c                   else
047200080912      * mi salvo i dati della nazione che mi servono dopo
047300080912     c                   eval      svtbluni = tbluni
047400000000E   2C                   ENDIF
047500000000E   1C                   ENDIF
047600000000     C*--------------------
047700970704     C* CAP
047800000000     C*--------------------
047900000000IF  1C     $ERR          IFEQ      '0'
048000970714IF  2C     V1CCAD        IFGT      *BLANKS
048100090615     c                   clear                   tisi97ds
048200970704     C                   MOVEL     V1CNAR        WNAR
048300970704     C                   MOVEL     V1CCAD        WCAP
048400090615     c
048500970704     C                   EXSR      CHKCAP
048600970704     C                   MOVEL     WNAR          V1CNAR
048700970704     C                   MOVEL     WCAP          V1CCAD
048800970714IF  3C     $ERR          IFEQ      '1'
048900000000     C                   MOVEL     MSG(11)       $MSG
049000000000     C                   EXSR      CARMSG
049100970704     C                   SETON                                        6263
049200970714E   3C                   ENDIF
049300970714E   2C                   ENDIF
049400000000E   1C                   ENDIF
049500090618IF  4C                   IF        $err='0'   and (v1ccad<>savcad
049600090618     c                                        or   v1cnar<>savnar)
049700090618     c                   clear                   forzacaputi
049800090618     c                   eval      savcad=v1ccad
049900090618     c                   eval      savnar=v1cnar
050000090618    4c                   endif
050100970704     C*--------------------
050200970704     C* LOCALITA'
050300970704     C*--------------------
050400970704IF  1C     $ERR          IFEQ      '0'
050500970714IF  2C     V1CLOD        IFGT      *BLANKS
050600970704     C                   MOVEL     V1CNAR        WNAR
050700970704     C                   MOVEL     V1CCAD        WCAP
050800970704     C                   MOVEL     V1CLOD        WLOC
050900970704     C                   EXSR      CHKLOC
051000970704     C                   MOVEL     WNAR          V1CNAR
051100970704     C                   MOVEL     WCAP          V1CCAD
051200970704     C                   MOVEL     WLOC          V1CLOD
051300970714E   2C                   ENDIF
051400970704E   1C                   ENDIF
051500970704     C*--------------------
051600970714     C* CAP + LOCALITA'
051700970704     C*--------------------
051800970714IF  1C     $ERR          IFEQ      '0'
051900970714IF  2C     V1CCAD        IFEQ      *BLANKS
052000970714     C                   MOVEL     '1'           $ERR
052100970714     C                   MOVEL     MSG(11)       $MSG
052200970714     C                   EXSR      CARMSG
052300970714     C                   SETON                                        6263
052400970714E   2C                   ENDIF
052500970714E   1C                   ENDIF
052600970704IF  1C     $ERR          IFEQ      '0'
052700090615     c                   clear                   tisi97ds
052800970704     C                   MOVEL     V1CNAR        WNAR
052900970704     C                   MOVEL     V1CCAD        WCAP
053000970704     C                   MOVEL     V1CLOD        WLOC
053100970704     C                   EXSR      CHKCLP
053200970704IF  2C     $ERR          IFEQ      '1'
053300970704     C                   MOVEL     MSG(14)       $MSG
053400970704     C                   EXSR      CARMSG
053500980116     C                   SETON                                        64
053600970704E   2C                   ENDIF
053700970704E   1C                   ENDIF
053800000000     C*--------------------
053900000000     C* TIPO SERVIZIO
054000000000     C*--------------------
054100000000IF  1C     $ERR          IFEQ      '0'
054200000000     C                   MOVEL     V1CTSP        WTSP
054300970704     C                   MOVEL     *BLANKS       WDTSP
054400000000     C                   EXSR      CHKTSP
054500000000     C                   MOVEL     WTSP          V1CTSP
054600000000     C                   MOVEL     WDTSP         V1DTSP
054700000000IF  2C     $ERR          IFEQ      '1'
054800000000     C                   MOVEL     MSG(9)        $MSG
054900000000     C                   EXSR      CARMSG
055000000000     C                   SETON                                        65
055100000000E   2C                   ENDIF
055200000000E   1C                   ENDIF
055300000000     C*--------------------
055400110623     C* FLAG inoltro
055500000000     C*--------------------
055600000000IF  1C     $ERR          IFEQ      '0'
055700110623     c     v1ciso        andne     *blanks
055800110623     C                   MOVEL     *BLANKS       V1DISO
055900110623     C                   MOVEL     V1cISO        WINL
056000110623     C                   MOVEL     *BLANKS       WDINL
056100110623     C                   EXSR      CHKINL
056200110623     C                   MOVEL     WINL          V1cISO
056300110623     C                   MOVEL     WDINL         V1DISO
056400110623IF  2C     $err          IFEQ      '1'
056500110623     C                   SETON                                        66
056600110623     C                   MOVEL     MSG(12)       $MSG
056700110623     C                   EXSR      CARMSG
056800110623X   2C                   ELSE
056900110623E   2C                   ENDIF
057000110623E   1C                   ENDIF
057100980116     C*--------------------
057200980116     C* FERMO DEPOSITO
057300980116     C*--------------------
057400980116IF  1C     $ERR          IFEQ      '0'
057500980116IF  2C     V1CFFD        IFEQ      'S'
057600980116     C     V1CFFD        OREQ      'N'
057700980116X   2C                   ELSE
057800980116     C                   MOVEL     '1'           $ERR
057900980116     C                   MOVEL     MSG(15)       $MSG
058000980116     C                   EXSR      CARMSG
058100980116     C                   SETON                                        78
058200980116E   2C                   ENDIF
058300980116E   1C                   ENDIF
058400050428     c*
058500050428     c* Non posso forzare inoltro I o P se fermo deposito='S'
058600050428IF  1C     $ERR          IFEQ      '0'
058700050428     c                   if        v1cffd='S' and v1ciso<>' ' and v1ciso<>'C'
058800050428     C                   MOVEL     '1'           $ERR
058900050428     C                   MOVEL     MSG(36)       $MSG
059000050428     C                   EXSR      CARMSG
059100050428     C                   SETON                                        78
059200050428E   2C                   ENDIF
059300050428E   1C                   ENDIF
059400000919     C*--------------------
059500000919     C* MERCE A MAGAZZINO
059600000919     C*--------------------
059700000919IF  1C     $ERR          IFEQ      '0'
059800000919IF  2C     V1CFDN        IFEQ      'S'
059900000919     C     V1CFDN        OREQ      'N'
060000000919X   2C                   ELSE
060100000919     C                   MOVEL     '1'           $ERR
060200000919     C                   MOVEL     MSG(15)       $MSG
060300000919     C                   EXSR      CARMSG
060400000919     C                   SETON                                        82
060500000919E   2C                   ENDIF
060600000919E   1C                   ENDIF
060700980703     C*--------------------
060800980703     C* TIPO PORTO
060900980703     C*--------------------
061000980703IF  1C     $ERR          IFEQ      '0'
061100980703IF  2C     V1CTPO        IFEQ      'F'
061200980703     C     V1CTPO        OREQ      'A'
061300980703X   2C                   ELSE
061400980703     C                   MOVEL     '1'           $ERR
061500980703     C                   MOVEL     MSG(13)       $MSG
061600980703     C                   EXSR      CARMSG
061700980703     C                   SETON                                        77
061800980703E   2C                   ENDIF
061900980703E   1C                   ENDIF
062000090616
062100090616    1c                   if        $err='0' and v1ctpo<>savtpo
062200090616     c                   movel     v1ctpo        savtpo
062300090616     c                   clear                   forzaPA
062400090616     c                   clear                   forzaCAPuti
062500090616    1c                   endif
062600090617     c
062700090617IF  4C                   IF        $err='0'   and v1ccas<>savcas
062800090617     c                   clear                   forzacaputi
062900090617     c                   eval      savcas=v1ccas
063000090617    4c                   endif
063100980703     C***
063200980703     C* DATI MITTENTE OBBLIGATORI, SE PORTO ASSEGNATO
063300980703     C***
063400980703IF  1C     $ERR          IFEQ      '0'
063500980703IF  2C     V1CTPO        IFEQ      'F'                                          *FRANCO
063600980703IF  3C     V1CNZM        IFNE      *BLANKS
063700980703     C     V1CCAM        ORNE      *BLANKS
063800980703     C     V1CLOM        ORNE      *BLANKS
063900980703     C                   MOVEL     '1'           $ERR
064000980703     C                   MOVEL     MSG(28)       $MSG
064100980703     C                   EXSR      CARMSG
064200980703     C                   SETON                                        798081
064300980703E   3C                   ENDIF
064400980703X   2C                   ELSE                                                   *ASSEGNATO
064500980703     C*--------------------
064600980703     C* NAZIONE MITTENTE
064700980703     C*--------------------
064800980703IF  3C     $ERR          IFEQ      '0'
064900980703     C                   MOVEL     V1CNZM        WNAR
065000980703     C                   MOVEL     *BLANKS       WDNAR
065100980703     C                   EXSR      CHKNAR
065200980703     C                   MOVEL     WNAR          V1CNZM
065300980703IF  4C     $ERR          IFEQ      '1'
065400980703     C                   MOVEL     MSG(10)       $MSG
065500980703     C                   EXSR      CARMSG
065600980703     C                   SETON                                        79
065700980703E   4C                   ENDIF
065800980703E   3C                   ENDIF
065900980703     C*--------------------
066000980703     C* CAP MITTENTE
066100980703     C*--------------------
066200980703IF  3C     $ERR          IFEQ      '0'
066300980703IF  4C     V1CCAM        IFGT      *BLANKS
066400090615     c                   clear                   tisi97ds
066500980703     C                   MOVEL     V1CNZM        WNAR
066600980703     C                   MOVEL     V1CCAM        WCAP
066700980703     C                   EXSR      CHKCAP
066800980703     C                   MOVEL     WNAR          V1CNZM
066900980703     C                   MOVEL     WCAP          V1CCAM
067000980703IF  5C     $ERR          IFEQ      '1'
067100980703     C                   MOVEL     MSG(11)       $MSG
067200980703     C                   EXSR      CARMSG
067300980703     C                   SETON                                        7980
067400980703E   5C                   ENDIF
067500980703E   4C                   ENDIF
067600980703E   3C                   ENDIF
067700980703     C*--------------------
067800980703     C* LOCALITA' MITTENTE
067900980703     C*--------------------
068000980703IF  3C     $ERR          IFEQ      '0'
068100980703IF  4C     V1CLOM        IFGT      *BLANKS
068200980703     C                   MOVEL     V1CNZM        WNAR
068300980703     C                   MOVEL     V1CCAM        WCAP
068400980703     C                   MOVEL     V1CLOM        WLOC
068500980703     C                   EXSR      CHKLOC
068600980703     C                   MOVEL     WNAR          V1CNZM
068700980703     C                   MOVEL     WCAP          V1CCAM
068800980703     C                   MOVEL     WLOC          V1CLOM
068900980703E   4C                   ENDIF
069000980703E   3C                   ENDIF
069100980703     C*--------------------
069200980703     C* CAP + LOCALITA' MITTENTE
069300980703     C*--------------------
069400980703IF  3C     $ERR          IFEQ      '0'
069500980703IF  4C     V1CCAM        IFEQ      *BLANKS
069600980703     C                   MOVEL     '1'           $ERR
069700980703     C                   MOVEL     MSG(11)       $MSG
069800980703     C                   EXSR      CARMSG
069900980703     C                   SETON                                        7980
070000980703E   4C                   ENDIF
070100980703E   3C                   ENDIF
070200980703IF  3C     $ERR          IFEQ      '0'
070300980703     C                   MOVEL     V1CNZM        WNAR
070400980703     C                   MOVEL     V1CCAM        WCAP
070500980703     C                   MOVEL     V1CLOM        WLOC
070600980703     C                   EXSR      CHKCLP
070700980703IF  4C     $ERR          IFEQ      '1'
070800980703     C                   MOVEL     MSG(14)       $MSG
070900980703     C                   EXSR      CARMSG
071000980703     C                   SETON                                        81
071100980703E   4C                   ENDIF
071200980703E   3C                   ENDIF
071300980703E   2C                   ENDIF
071400980703E   1C                   ENDIF
071500020225     C*--------------------
071600020225     C* NETWORK
071700020225     C*--------------------
071800020225IF  1C     $ERR          IFEQ      '0'
071900080912     c                   movel     svtbluni      ds15
072000090615    2C     V1CNAR        IFEQ      *BLANKS
072100090615    3C     V1CNTW        IFNE      *BLANKS
072200020225     C                   MOVEL     MSG(16)       $MSG
072300020225     C                   EXSR      CARMSG
072400020225     C                   SETON                                        6283
072500090615    3C                   ENDIF
072600090618     c                   clear                   Dntw
072700090618     c                   clear                   v1Dntw
072800120509     c                   if        savntw<>v1cntw
072900120509     c                   clear                   forzaIE
073000120509     c                   clear                   forzaCAPuti
073100120509     c                   movel     v1cntw        savntw
073200120509     c                   endif
073300090506     c
073400090615   x2C                   ELSE
073500020225     C                   EXSR      CHKNTW
073600090615IF  3C     $ERR          IFEQ      '1'
073700020225     C                   MOVEL     MSG(31)       $MSG
073800020225     C                   EXSR      CARMSG
073900020225     C                   SETON                                        83
074000090615   x3c                   else
074100090615     c* controllo cappario utilizzi particolari
074200090615     c                   clear                   tisi97ds
074300090615     c                   eval      i97ntw=v1cntw
074400090615     C                   MOVEL     V1CNAR        WNAR
074500090615     C                   MOVEL     V1CCAD        WCAP
074600090615     C                   MOVEL     V1CLOD        WLOC
074700090615     C                   EXSR      CHKCLP
074800090616     c
074900090616     c* errore forzabile per cappario utilizzi particolari
075000090616     c                   if        $err='1' and forzacaputi=' '
075100090615     C                   MOVEL     o13msg        $MSG
075200090615     C                   EXSR      CARMSG
075300090615     C                   SETON                                        83
075400090616     c                   eval      Forzacaputi='S'
075500090615E   4C                   ENDIF
075600090616     c
075700090615E   3C                   ENDIF
075800090615    2C                   ENDIF
075900020225E   1C                   ENDIF
076000020514     C*--------------------
076100020514     C* NAZIONE IN RELAZIONE AL NETWORK
076200020514     C*--------------------
076300020514IF  1C     $ERR          IFEQ      '0'
076400020514     C     V1CNAR        IFNE      *BLANKS
076500020514     C     V1CNTW        ANDNE     *BLANKS
076600020514     C* CONTROLLO X DPD
076700020514     C     V1CNTW        IFEQ      'DPD'
076800020514     C     §15LAD        ANDEQ     *ZEROS
076900020514     C                   SETON                                        6283
077000020514     C                   MOVEL     '1'           $ERR
077100020514     C                   MOVEL     MSG(32)       $MSG
077200020514     C                   EXSR      CARMSG
077300020514     C                   ENDIF
077400020514     C* CONTROLLO X FED-EX
077500020514     C     V1CNTW        IFEQ      'FED'
077600020514     C     §15LAF        ANDEQ     *ZEROS
077700020514     C                   SETON                                        6283
077800020514     C                   MOVEL     '1'           $ERR
077900020514     C                   MOVEL     MSG(32)       $MSG
078000020514     C                   EXSR      CARMSG
078100020514     C                   ENDIF
078200020514     C* CONTROLLO X FED-EX
078300020514     C     V1CNTW        IFEQ      'EEX'
078400020514     C     §15EEX        ANDNE     'S'
078500020514     C                   SETON                                        6283
078600020514     C                   MOVEL     '1'           $ERR
078700020514     C                   MOVEL     MSG(32)       $MSG
078800020514     C                   EXSR      CARMSG
078900020514     C                   ENDIF
079000020514     C                   ENDIF
079100020514E   1C                   ENDIF
079200050413     C*--------------------
079300050413     C* CONSEGNE PARTICOLARI
079400050413     C*--------------------
079500050413IF  1C     $ERR          IFEQ      '0'
079600050413     C                   MOVEL     V1CTC1        WTCX
079700050413     C                   MOVEL     *BLANKS       WDTCX
079800050413     C                   EXSR      CHKtcx
079900050413     C                   MOVEL     Wtcx          V1Ctc1
080000050413     C                   MOVEL     WDtcx         V1Dtc1
080100050413IF  2C     $ERR          IFEQ      '1'
080200050413     C                   MOVEL     MSG(34)       $MSG
080300050413     C                   EXSR      CARMSG
080400050413     C                   SETON                                        86
080500050413E   2C                   ENDIF
080600050413E   1C                   ENDIF
080700050413     c
080800050413IF  1C     $ERR          IFEQ      '0'
080900050413     C                   MOVEL     V1CTC2        WTCX
081000050413     C                   MOVEL     *BLANKS       WDTCX
081100050413     C                   EXSR      CHKtcx
081200050413     C                   MOVEL     Wtcx          V1Ctc2
081300050413     C                   MOVEL     WDtcx         V1Dtc2
081400050413IF  2C     $ERR          IFEQ      '1'
081500050413     C                   MOVEL     MSG(34)       $MSG
081600050413     C                   EXSR      CARMSG
081700050413     C                   SETON                                        87
081800050413E   2C                   ENDIF
081900050413E   1C                   ENDIF
082000050413     c* Devono essere <>
082100050413IF  1C     $ERR          IFEQ      '0'
082200050413     c                   if        v1ctc1 = v1ctc2 and v1ctc1<>' '
082300050413     c                   eval      $err='1'
082400050413     C                   MOVEL     MSG(35)       $MSG
082500050413     C                   EXSR      CARMSG
082600050413     C                   SETON                                        87
082700050413E   2C                   ENDIF
082800050413     c                   endif
082900960528     C***
083000960528     C* SIMULAZIONE TASSAZIONE
083100960528     C***
083200960729     C* SE I COLLI SONO = 0 NON ESEGUE LA TASSAZIONE
083300960729     C* SE I COLLI SONO > 0 RICERCA I DATI DI TASSAZIONE
083400960703     C*--------------------
083500000919     C* SE IL CLIENTE È VUOTO, IMPOSTA LA TARIFFA DI CARTELLO
083600960703     C*--------------------
083700120509 F  1C     $ERR          IFEQ      '0'
083800020313     C                   MOVEL     V1CKSC        DEPCART           5 0
083900020313IF  2C                   IF        V1CNCL  <> *ZEROS AND
084000020313     C                             (V1CKSC  = *ZEROS OR
084100020313     C                              DEPCART = 88888)
084200021104     C*
084300021104     C* REIMPOSTO I DATI RELATIVI ALLA TARIFFA DI CARTELLO SOLO SE NN TRATTASI DI FED-EX
084400120608     c*  o se cambiato il peso
084500160302     C                   IF        savt_NTW = 'FED' AND v1cNTW = 'FED'
084600120608     c                             and v1cpkb=savpkb
084700021104     C                   ELSE
084800020226     C* 1 VOLTA REPERITO/CONTROLLATO IL NETWORK DETERMINO LA TARIFFA DI CARTELLO
084900020226     C                   EXSR      CARTAR
085000160302     c                   move      oc7kscc       alfaksc           7
085100160302     c                   movel     oc7ctrc       alfactr           3
085200120608     c**                 if        alfaksc<>v1cksc or alfactr<>v1cctr
085300120608     c**                 eval      $err='1'
085400120608     C**                 ENDIF
085500160302     C                   MOVE      Oc7KSCc       V1CKSC
085600160302     C                   MOVEL     Oc7CTRc       V1CCTR
085700120608     C                   MOVEL     V1CCTR        WCTR
085800120608     C                   EXSR      CHKCTR
085900020226     C***                MOVEL     *ZEROS        V1CCTR
086000120608     c                   eval      savpkb=v1cpkb
086100160302     c                   eval      savt_ntw=v1cntw
086200120608     c
086300120608     C                   ENDIF
086400960729E   2C                   ENDIF
086500960729E   1C                   ENDIF
086600120509     C*----------------------------
086700120509     C* COLLI e Peso rispetto a DPD
086800120509     C*----------------------------
086900120509 F  1C     $ERR          IFEQ      '0'
087000120509      * OBBLIGATORIO 1 COLLO
087100120509     C     §OGNTW        IFEQ      'DPD'
087200120509     C     v1cntw        oreq      'DPD'
087300120509     C     V1CNCL        ifne      1
087400120509     C                   MOVEL     '1'           $ERR
087500120509     C                   MOVEL     MSG(47)       $MSG
087600120509     C                   EXSR      CARMSG
087700120509     C                   SETON                                        67
087800120509     C                   ENDIF
087900120509     C                   ENDIF
088000120509    1C                   ENDIF
088100120509      * Massimo peso inseribile --> da tabella
088200120509 F  1C     $ERR          IFEQ      '0'
088300120514     C                   if        (v1cntw='DPD' or §ogntw='DPD')
088400120509     c                             and v1cpkb>§3ipkd
088500120509     C                   MOVEL     '1'           $ERR
088600120509     C                   MOVEL     MSG(48)       $MSG
088700120509     c                   eval      $msg = %trim($msg) + ' ' +
088800120509     c                             %trim(%editc(§3ipkd:'2'))+
088900120509     c                             ' Kg.'
089000120509     C                   EXSR      CARMSG
089100120509     C                   SETON                                        68
089200120509     C                   ENDIF
089300120509    1c                   endif
089400960528     C*--------------------
089500960528     C* PESO
089600960528     C*--------------------
089700960528IF  1C     $ERR          IFEQ      '0'
089800960729IF  2C     V1CNCL        IFGT      *ZEROS
089900960729IF  3C     V1CPKB        IFLE      *ZEROS
090000960528     C                   MOVEL     '1'           $ERR
090100960528     C                   MOVEL     MSG(18)       $MSG
090200960528     C                   EXSR      CARMSG
090300960528     C                   SETON                                        68
090400960729E   3C                   ENDIF
090500960729X   2C                   ELSE
090600960729     C                   Z-ADD     *ZEROS        V1CPKB
090700960729E   2C                   ENDIF
090800960528E   1C                   ENDIF
090900960528     C*--------------------
091000960528     C* VOLUME
091100960528     C*--------------------
091200960528IF  1C     $ERR          IFEQ      '0'
091300960729IF  2C     V1CNCL        IFGT      *ZEROS
091400960729IF  3C     V1CVLF        IFLE      *ZEROS
091500960528     C                   MOVEL     '1'           $ERR
091600960528     C                   MOVEL     MSG(19)       $MSG
091700960528     C                   EXSR      CARMSG
091800960528     C                   SETON                                        69
091900960729E   2C                   ENDIF
092000960729X   2C                   ELSE
092100960729     C                   Z-ADD     *ZEROS        V1CVLF
092200960528E   2C                   ENDIF
092300960528E   1C                   ENDIF
092400110923     C*--------------------
092500110923     C* Limiti colli/peso/volume
092600110923     C*--------------------
092700110923IF  1C     $ERR          IFEQ      '0'
092800110923     c*
092900110923     c* campi per la forzatura del messaggio di errore --> pulizia a cambio di tipo servizio
093000110923     c                   if        v1ctsp<>savtsp
093100110923     c                   clear                   wncl_pre
093200110923     c                   clear                   wpes_pre
093300110923     c                   clear                   wvol_pre
093400110923     c                   eval      savtsp=v1ctsp
093500110923     c                   endif
093600110923     c*
093700110923     c                   clear                   trul73ds
093800110923     c                   eval      i73tsp=v1ctsp
093900110923     c                   eval      i73psk=v1cpkb
094000110923     c                   eval      i73vmc=v1cvlf
094100110923     c                   eval      i73ncl=v1cncl
094200110923     c                   call      'TRUL73R'
094300110923     c                   parm                    trul73ds
094400110923     c                   select
094500110923     c* colli
094600110927     c                   when      o73fclm='1' and v1cncl<>wncl_pre
094700110923     C                   MOVEL     '1'           $ERR
094800110923     c                   eval      $msg=msg(46)
094900110923     C                   EXSR      CARMSG
095000110923     c                   seton                                        67
095100110923     c                   eval      wncl_pre=v1cncl
095200110923     c* peso kg
095300110927     c                   when      O73FKGM='1' and v1cpkb<>wpes_pre
095400110923     C                   MOVEL     '1'           $ERR
095500110923     c                   eval      $msg=msg(46)
095600110923     C                   EXSR      CARMSG
095700110923     c                   seton                                        68
095800110923     c                   eval      wpes_pre=v1cpkb
095900110923     c*volume mc
096000110927     c                   when      O73FMCM='1' and v1cvlf<>wvol_pre
096100110923     C                   MOVEL     '1'           $ERR
096200110923     c                   eval      $msg=msg(46)
096300110923     C                   EXSR      CARMSG
096400110923     c                   seton                                        69
096500110923     c                   eval      wvol_pre=v1cvlf
096600110923     c                   endsl
096700110923E   1C                   ENDIF
096800960614     C*--------------------
096900960614     C* CLIENTE
097000960614     C*--------------------
097100960703IF  1C     $ERR          IFEQ      '0'
097200960729IF  2C     V1CNCL        IFGT      *ZEROS
097300960614     C                   MOVEL     V1CKSC        WKSC
097400960614     C                   MOVEL     *BLANKS       WDKSC
097500960614     C                   EXSR      CHKKSC
097600960614     C                   MOVEL     WKSC          V1CKSC
097700960614     C                   MOVEL     WDKSC         V1DKSC
097800960729IF  3C     $ERR          IFEQ      '1'
097900960614     C                   MOVEL     MSG(20)       $MSG
098000960614     C                   EXSR      CARMSG
098100960614     C                   SETON                                        70
098200960729E   3C                   ENDIF
098300960729X   2C                   ELSE
098400960729     C                   MOVEL     *ZEROS        V1CKSC
098500960729E   2C                   ENDIF
098600960703E   1C                   ENDIF
098700960614     C*--------------------
098800960614     C* TARIFFA
098900960614     C*--------------------
099000960703IF  1C     $ERR          IFEQ      '0'
099100090429     c
099200960729IF  2C     V1CNCL        IFGT      *ZEROS
099300960614     C                   MOVEL     V1CCTR        WCTR
099400960614     C                   EXSR      CHKCTR
099500960729IF  3C     $ERR          IFEQ      '1'
099600960729E   3C                   ENDIF
099700960729X   2C                   ELSE
099800960729     C                   MOVEL     *ZEROS        V1CCTR
099900960729E   2C                   ENDIF
100000960703E   1C                   ENDIF
100100010823     C*--------------------
100200960925     C* C/ASSEGNO
100300010823     C*--------------------
100400010823     C*=======
100500010823     C* DIVISA E/O TIPO INCASSO IMPOSTATI MA MANCA IMPORTO CONTRASSEGNO
100600010823     C*=======
100700010823IF  1C     $ERR          IFEQ      '0'
100800010823IF  2C     V1CVCA        IFNE      *BLANKS
100900010823     C     V1CTIC        ORNE      *BLANKS
101000010823IF  3C     V1CCAS        IFEQ      *ZEROS
101100010823     C                   MOVEL     '1'           $ERR
101200010823     C                   MOVEL     MSG(24)       $MSG
101300010823     C                   EXSR      CARMSG
101400010823     C                   SETON                                        72
101500010823E   3C                   ENDIF
101600010823E   2C                   ENDIF
101700010823E   1C                   ENDIF
101800010823     C*
101900010823     C*=======
102000010823     C* DIVISA
102100010823     C*=======
102200010823IF  1C     $ERR          IFEQ      '0'
102300010823IF  2C     V1CCAS        IFGT      *ZEROS
102400010823     C                   MOVEL     V1CVCA        WDIV
102500010823     C                   EXSR      CHKDIV
102600010823     C                   MOVEL     WDIV          V1CVCA
102700090616IF  3C     $ERR          IFEQ      '1'
102800010829     C     V1CVCA        OREQ      *BLANKS
102900010823     C                   MOVEL     MSG(22)       $MSG
103000010823     C                   EXSR      CARMSG
103100010823     C                   SETON                                        73
103200090616E   3C                   ENDIF
103300010823     C*=======
103400010823     C* TIPO INCASSO
103500010823     C*=======
103600090616IF  3C     $ERR          IFEQ      '0'
103700010823     C                   MOVEL     V1CTIC        WTIC
103800010823     C                   MOVEL     *BLANKS       WUDE
103900010823     C                   EXSR      CHKTIC
104000010823     C                   MOVEL     WTIC          V1CTIC
104100090616IF  4C     $ERR          IFEQ      '1'
104200010823     C                   MOVEL     MSG(23)       $MSG
104300010823     C                   EXSR      CARMSG
104400010823     C                   SETON                                        74
104500090616E   4C                   ENDIF
104600090616     c
104700090617E   3C                   ENDIF
104800010823E   2C                   ENDIF
104900960926E   1C                   ENDIF
105000960926     C*--------------------
105100960926     C* VALUTA + IMPORTO
105200960926     C*--------------------
105300960926IF  1C     $ERR          IFEQ      '0'
105400960926IF  2C     WFDC          IFNE      'S'
105500960926     C                   MOVE      V1CCAS        WN33              3 3
105600960926IF  3C     WN33          IFNE      *ZEROS
105700960926     C                   MOVEL     '1'           $ERR
105800960926     C                   MOVEL     MSG(25)       $MSG
105900960926     C                   EXSR      CARMSG
106000960926     C                   SETON                                        7273
106100960926E   3C                   ENDIF
106200960926E   2C                   ENDIF
106300960926E   1C                   ENDIF
106400090617
106500960926     C*---
106600960926     C* ASSICURAZIONE
106700960926     C*---
106800960926     C*--------------------
106900960926     C* VALUTA
107000960926     C*--------------------
107100010823IF  1C     $ERR          IFEQ      '0'
107200010823IF  2C     V1CIAS        IFGT      *ZEROS
107300960926     C                   MOVEL     *BLANKS       WFDC
107400960926     C                   MOVEL     V1CVAS        WDIV
107500960926     C                   EXSR      CHKDIV
107600960926     C                   MOVEL     WDIV          V1CVAS
107700170120IF  3C     $ERR          IFEQ      '1'
107800010829     C     V1CVAS        OREQ      *BLANKS
107900960926     C                   MOVEL     MSG(22)       $MSG
108000960926     C                   EXSR      CARMSG
108100960926     C                   SETON                                        76
108200170120   x3c                   else
108300170120     c* Verifico e rientra nei limiti
108400170120     c                   clear                   trul22ds
108500170120IF  4c     v1ctpo        IFEQ      'F'                                          *FRANCO->DATI DESTIN
108600170120     C                   MOVEL     'F1'          i22cbo                         *TIPO BOLLA
108700170120X    C                   ELSE                                                   *ASSEGNATO->DATI MIT
108800170120     C                   MOVEL     'A2'          i22cbo
108900170120    4c                   endif
109000170120     C                   MOVEL     v1ctsp        i22tsp
109100170120     C                   movel     v1clnp        i22LNP                         *LINEA PARTENZA
109200170120     C                   MOVEL     v1cnzm        i22NZM                         *NAZIONE MITTENTE
109300170120     c                   exsr      callna
109400170120     C                   Z-ADD     O95LNA        i22LNA                         *LINEA ARRIVO
109500170120     C                   MOVEL     v1cnar        i22NZD                         *NAZIONE DESTINO
109600170120     C                   MOVEL     v1cksc        i22KSC
109700170120     C                   MOVEL     v1cctr        i22CTR
109800170120     C                   Z-ADD     v1cias        i22IMP                         *ASSICURAZIONE
109900170120     C                   MOVEL     v1cvas        i22div                         *DIVISA NON LA SO
110000170120     c                   call      'TRUL22R'
110100170120     c                   parm                    trul22ds
110200170120
110300170120     C* IMPORTO DA ASSICUARE NON AMMESSO
110400170120     C     O22FMN        IFNE      *BLANKS
110500170120    4C     O22FX2        orne      *BLANKS
110600170120     C                   MOVEL     '1'           $ERR
110700170120     C                   MOVEL     MSG(49)       $MSG
110800170120     C                   EXSR      CARMSG
110900170120     C                   SETON                                        75
111000170120    4c                   endif
111100170120
111200170120E   3C                   ENDIF
111300010829E   2C                   ENDIF
111400010829E   1C                   ENDIF
111500960926     C*--------------------
111600960926     C* IMPORTO
111700960926     C*--------------------
111800960926IF  1C     $ERR          IFEQ      '0'
111900960926IF  2C     V1CVAS        IFNE      *BLANKS
112000960926IF  3C     V1CIAS        IFEQ      *ZEROS
112100960926     C                   MOVEL     '1'           $ERR
112200960926     C                   MOVEL     MSG(27)       $MSG
112300960926     C                   EXSR      CARMSG
112400960926     C                   SETON                                        75
112500960926E   3C                   ENDIF
112600960926E   2C                   ENDIF
112700960926E   1C                   ENDIF
112800960926     C*--------------------
112900960926     C* VALUTA + IMPORTO
113000960926     C*--------------------
113100960926IF  1C     $ERR          IFEQ      '0'
113200960926IF  2C     WFDC          IFNE      'S'
113300960926     C                   MOVE      V1CIAS        WN33              3 3
113400960926IF  3C     WN33          IFNE      *ZEROS
113500960926     C                   MOVEL     '1'           $ERR
113600960926     C                   MOVEL     MSG(25)       $MSG
113700960926     C                   EXSR      CARMSG
113800960926     C                   SETON                                        7576
113900960926E   3C                   ENDIF
114000960926E   2C                   ENDIF
114100960926E   1C                   ENDIF
114200021106     C*--------------------
114300021106     C* CONSEGNA DDT
114400021106     C*--------------------
114500021106IF  1C     $ERR          IFEQ      '0'
114600021106IF  2C     V1CDDT        IFEQ      'S'
114700021106     C     V1CDDT        OREQ      'N'
114800021106X   2C                   ELSE
114900021106     C                   MOVEL     '1'           $ERR
115000021106     C                   MOVEL     MSG(15)       $MSG
115100021106     C                   EXSR      CARMSG
115200021106     C                   SETON                                        85
115300021106E   2C                   ENDIF
115400021106E   1C                   ENDIF
115500110817     C*----------------------
115600110817     C* EMAIL AL DESTINATARIO
115700110817     C*----------------------
115800110817IF  1C     $ERR          IFEQ      '0'
115900110817IF  2C     V1CEMD        IFEQ      'S'
116000110817     C     V1CEMD        OREQ      'N'
116100130925     C     V1CEMD        OREQ      'M'
116200110817X   2C                   ELSE
116300110817     C                   MOVEL     '1'           $ERR
116400110817     C                   MOVEL     MSG(15)       $MSG
116500110817     C                   EXSR      CARMSG
116600110817     C                   SETON                                        88
116700110817E   2C                   ENDIF
116800110817E   1C                   ENDIF
116900960926     C*
117000960926     C                   ENDSR
117100000000     C*------------------------------------------------------------------------*
117200000000     C* CHKDA8 - CONTROLLA UNA DATA LUNGA 8 (G/M/A) E LA INVERTE (A/M/G)
117300000000     C*------------------------------------------------------------------------*
117400000000     C     CHKDA8        BEGSR
117500000000     C*
117600000000     C     *LIKE         DEFINE    G08DAT        WDA8
117700000000     C     *LIKE         DEFINE    G08INV        DA8W
117800000000     C*
117900000000     C                   Z-ADD     WDA8          G08DAT
118000000000     C                   Z-ADD     *ZEROS        G08INV
118100000000     C                   MOVEL     '0'           G08ERR
118200000000     C                   CALL      'XSRDA8'
118300000000     C                   PARM                    WLBDA8
118400000000     C                   Z-ADD     G08DAT        WDA8
118500000000IF  1C     G08ERR        IFEQ      '1'
118600000000     C                   MOVEL     '1'           $ERR
118700000000X   1C                   ELSE
118800000000     C                   Z-ADD     G08INV        DA8W
118900000000E   1C                   ENDIF
119000000000     C*
119100000000     C                   ENDSR
119200000000     C*------------------------------------------------------------------------*
119300000000     C* CHKLNP - CONTROLLA LA FILIALE
119400000000     C*------------------------------------------------------------------------*
119500000000     C     CHKLNP        BEGSR
119600000000     C*
119700000000     C     *LIKE         DEFINE    V1CLNP        WLNP
119800000000     C     *LIKE         DEFINE    V1DLNP        WDLNP
119900000000     C*
120000970704     C* RICHIESTO ELENCO
120100000000     C     '?'           SCAN      WLNP                                   99
120200000000IF  1C     *IN99         IFEQ      *ON
120300000000     C                   MOVEL     *BLANKS       §TIP              1
120400960926     C                   MOVEL     *BLANKS       §FIL              3
120500960926     C                   MOVE      *BLANKS       §DEC             25
120600971203     C                   MOVE      *BLANKS       §PRA              3
120700971203     C                   CALL      'TNSD24R'
120800960926     C                   PARM                    §FIL
120900000000     C                   PARM                    §TIP
121000960926     C                   PARM                    §DEC
121100971203     C                   PARM                    §PRA
121200960926     C                   MOVEL     §FIL          WLNP
121300960926     C                   MOVEL     §DEC          WDLNP
121400970704     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
121500970704E   1C                   ENDIF
121600000000     C*
121700970704     C* SOLO CARATTERI NUMERICI
121800000000     C                   SETOFF                                       28
121900000000     C                   TESTN                   WLNP                 28
122000970704IF  1C     *IN28         IFEQ      *ON                                          DA TESTN E'NUMERO
122100000000     C                   MOVE      WLNP          W001              1
122200970704IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
122300000000     C                   MOVEL     '1'           $ERR
122400970704E   2C                   ENDIF
122500970704X   1C                   ELSE                                                   DA TESTN E'UN NUMERO
122600000000     C                   MOVEL     '1'           $ERR
122700970704E   1C                   ENDIF
122800000000     C*
122900970704     C* CONTROLLA ESISTENZA CODICE
123000970704IF  1C     $ERR          IFEQ      '0'
123100000000     C                   MOVEL     WLNP          KORFIL
123200120509     c                   clear                   og143
123300000000     C     KEYORG        CHAIN     AZORG01L                           99
123400970704IF  2C     *IN99         IFEQ      *OFF
123500000000     C     ORGFVA        ANDEQ     *BLANKS
123600000000     C     ORGFAG        ANDNE     'V'
123700000000     C                   MOVEL     ORGDES        WDLNP
123800120509     C                   MOVEL     ORGDE3        OG143
123900970704X   2C                   ELSE
124000000000     C                   MOVEL     '1'           $ERR
124100000000     C                   MOVEL     *ALL'*'       WDLNP
124200970704E   2C                   ENDIF
124300000000E   1C                   ENDIF
124400000000     C*
124500000000     C                   ENDSR
124600000000     C*------------------------------------------------------------------------*
124700000000     C* CHKNAR - CONTROLLA LA NAZIONE
124800000000     C*------------------------------------------------------------------------*
124900000000     C     CHKNAR        BEGSR
125000000000     C*
125100000000     C     *LIKE         DEFINE    V1CNAR        WNAR
125200000000     C     *LIKE         DEFINE    V1DNAR        WDNAR
125300000000     C*
125400970704     C* RICHIESTO ELENCO
125500000000     C     '?'           SCAN      WNAR                                   99
125600000000IF  1C     *IN99         IFEQ      *ON
125700020919     C                   MOVEL     1             §KUT              1 0
125800000000     C                   MOVEL     '15'          §TAB              2
125900000000     C                   MOVEL     *BLANKS       §KEY              8
126000960926     C                   MOVEL     *BLANKS       §DES             30
126100000000     C                   CALL      'X§TABER'
126200000000     C                   PARM                    §KUT
126300000000     C                   PARM                    §TAB
126400000000     C                   PARM                    §KEY
126500960926     C                   PARM                    §DES
126600000000     C                   MOVEL     §KEY          WNAR
126700960926     C                   MOVEL     §DES          WDNAR
126800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
126900970704E   1C                   ENDIF
127000000000     C*
127100970704     C* CONTROLLA ESISTENZA CODICE
127200020919     C                   MOVEL     1             KTBKUT
127300000000     C                   MOVEL     '15'          KTBCOD
127400000000     C                   MOVEL     *BLANKS       KTBKEY
127500000000     C                   MOVEL     WNAR          KTBKEY
127600000000     C     KEYTAB        CHAIN     TABEL00F                           99
127700970704IF  1C     *IN99         IFEQ      *OFF
127800000000     C     TBLFLG        ANDEQ     *BLANKS
127900000000     C                   MOVEL     TBLUNI        DS15
128000000000     C                   MOVEL     §15DES        WDNAR
128100970704X   1C                   ELSE
128200000000     C                   MOVEL     '1'           $ERR
128300000000     C                   MOVEL     *ALL'*'       WDNAR
128400000000E   1C                   ENDIF
128500000000     C*
128600000000     C                   ENDSR
128700020225     C*------------------------------------------------------------------------*
128800020225     C* CHKNTW - CONTROLLA IL NETWORK
128900020225     C*------------------------------------------------------------------------*
129000020225     C     CHKNTW        BEGSR
129100020225     C*
129200020225     C* RICHIESTO ELENCO
129300020225     C     '?'           SCAN      V1CNTW                                 99
129400020225     C* SCELTA VALORE DA TABELLA
129500020225IF  1C     *IN99         IFEQ      *ON
129600020925     C                   CLEAR                   TIBS02DS
129700020225     C                   MOVEL     'NTW'         T02COD
129800020225     C                   MOVEL     KNSIF         T02SIF
129900020225     C                   MOVEL     'R'           T02MOD
130000020225     C                   CALL      'TIBS02R'
130100020225     C                   PARM                    KPJBA
130200020925     C                   PARM                    TIBS02DS
130300020225     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
130400020225     C*
130500020225     C                   ELSE
130600020225     C* VERIFICA VALORE IN TABELLA
130700020925     C                   CLEAR                   TIBS02DS
130800020225     C                   MOVEL     'NTW'         T02COD
130900020225     C                   MOVEL     KNSIF         T02SIF
131000020225     C                   MOVEL     V1CNTW        T02KE1
131100020225     C                   MOVEL     'C'           T02MOD
131200020225     C                   CALL      'TIBS02R'
131300020225     C                   PARM                    KPJBA
131400020925     C                   PARM                    TIBS02DS
131500020225E   1C                   ENDIF
131600020225     C*
131700020225     C     T02ERR        IFEQ      *BLANKS
131800020225     C                   MOVEL     T02UNI        DNTW
131900020226     C                   MOVEL     T02KE1        V1CNTW
132000020226     C                   MOVEL     §NTWDES       V1DNTW
132100020225     C     §NTWFIE       IFEQ      'E'
132200020225     C                   ELSE
132300020225     C                   MOVEL     '1'           $ERR
132400020225     C                   ENDIF
132500020225     C                   ELSE
132600020225     C                   MOVEL     '1'           $ERR
132700020225     C                   ENDIF
132800090506     c                   if        savntw<>v1cntw
132900090506     c                   clear                   forzaIE
133000090616     c                   clear                   forzaCAPuti
133100090506     c                   movel     v1cntw        savntw
133200090506     c                   endif
133300020225     C*
133400020225     C                   ENDSR
133500050413     C*------------------------------------------------------------------------*
133600050413     C* CHKTCx - CONTROLLA consegna particolare
133700050413     C*------------------------------------------------------------------------*
133800050413     C     CHKTCx        BEGSR
133900050413    1C     wtcx          IFNE      ' '
134000050413     C* RICERCA
134100050413    2C     wtcx          IFEQ      '?'
134200050413     C                   MOVEL     1             §KUT              1 0
134300050413     C                   MOVEL     '1P'          §TAB              2
134400050413     C                   MOVEL     *BLANKS       §KEY              8
134500050413     C                   MOVEL     *BLANKS       §DES             30
134600050413     C                   CALL      'X§TABER'
134700050413     C                   PARM                    §KUT
134800050413     C                   PARM                    §TAB
134900050413     C                   PARM                    §KEY
135000050413     C                   PARM                    §DES
135100050413     C                   MOVEL     §KEY          WTcx
135200050413     C                   MOVEL     §DES          WDTCx
135300050413     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
135400050413    2C                   END
135500050413     C* CONTROLLO
135600050413     C                   MOVEL     1             KTBKUT
135700050413     C                   MOVEL     '1P'          KTBCOD
135800050413     C                   MOVEL(P)  WTCx          KTBKEY
135900050413     C     KEYTAB        CHAIN     TABEL00F                           99
136000050413IF  2C     *IN99         IFEQ      *OFF
136100050413     C     TBLFLG        ANDEQ     *BLANKS
136200050413     C                   MOVEL     tbluni        WDTCx
136300050413     C                   MOVEL     tbluni        ds1p
136400050413X   2C                   ELSE
136500050413     C                   MOVEL     '1'           $ERR
136600050413     C                   MOVEL     *ALL'*'       WDTCx
136700050413E   2C                   ENDIF
136800050413     c* Se non ' utilizzza<bile errore
136900050413     c                   if        $err='0' and
137000050413     c                             §1puar=' ' and §1pupr=' '
137100050413     C                   MOVEL     '1'           $ERR
137200050413     C                   MOVEL     *ALL'*'       WDTCx
137300050413     c                   endif
137400050413E   1C                   ENDIF
137500050413     c
137600050413     c                   endsr
137700000000     C*------------------------------------------------------------------------*
137800970704     C* CHKCAP - CONTROLLA IL CAP
137900000000     C*------------------------------------------------------------------------*
138000000000     C     CHKCAP        BEGSR
138100000000     C*
138200000000     C     *LIKE         DEFINE    V1CCAD        WCAP
138300970704     C*
138400970704     C* RICHIESTO ELENCO
138500970704     C                   SETOFF                                           99
138600970704     C     '?'           SCAN      WCAP                                   99
138700970704IF  1C     *IN99         IFEQ      *ON
138800970704     C                   CLEAR                   DSSI00
138900970704     C                   MOVEL     'C01'         D00OP0
139000970704     C                   Z-ADD     DSPV1C        D00DRI
139100970704     C                   CALL      'TISI02R'
139200000000     C                   PARM                    KPJBA
139300970704     C                   PARM                    DSSI00
139400970704IF  2C     D00ERR        IFNE      '1'
139500970704     C                   MOVEL     D00NAR        WNAR                           *NAZIONE
139600970704     C                   MOVEL     D00CAP        WCAP                           *CAP
139700970704E   2C                   ENDIF
139800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
139900970704E   1C                   ENDIF
140000970704     C*
140100970704     C* CONTROLLA ESISTENZA CODICE
140200090615     C                   CLEAR                   fnlv13ds
140300090615     C                   CLEAR                   DSSI95
140400970704     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
140500970704     C                   MOVEL     '3'           I95TCN                         *CONTROLLO
140600970704     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
140700120802
140800120802     c                   if        wnar='IRL'
140900120802     C                   MOVEL     'EIRE'        I95CAP                         *CAP
141000120802     c                   else
141100120802     C                   MOVEL     WCAP          I95CAP                         *CAP
141200120802     c                   endif
141300120802     c
141400970704     C                   Z-ADD     DSPV1C        I95DAT                         *DATA RIFERIMENTO
141500020225     C                   IF        V1CNTW = 'FED'
141600020225     C                   MOVEL     'S'           I95FI1
141700020225     C                   ENDIF
141800090615     C* ERRORE AFFIDABILITA = 0
141900090615     C                   MOVEL     ' '           I13AF0
142000090615     C                   CALL      'FNLV13R'
142100090615     C                   PARM                    KPJBA
142200090615     C                   PARM                    fnlv13ds
142300970704     C                   PARM                    DSSI95
142400090615     C                   PARM      ' '           flgbac            1
142500090615     C                   PARM                    tisi97ds
142600090615     c
142700970704IF  1C     O95ERR        IFNE      *BLANKS
142800090615     c     o13err        orne      *blanks
142900090629     c     o13rop        andeq     *blanks
143000970704     C                   MOVEL     '1'           $ERR
143100970704E   1C                   ENDIF
143200000000     C*
143300000000     C                   ENDSR
143400170120     C*------------------------------------------------------------------------*
143500170120     C* calcolo lna per richiamo a trul22r
143600170120     C*------------------------------------------------------------------------*
143700170120     C     callna        BEGSR
143800170120     C                   CLEAR                   DSSI95
143900170120     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
144000170120     C                   MOVEL     '3'           I95TCN                         *CONTROLLO FORMALE
144100170120     C                   MOVEL     v1cNAR        I95NAR                         *NAZIONE
144200170120     C                   MOVEL     v1cCAD        I95CAP                         *CAP
144300170120     c                   if        wnar='IRL'
144400170120     C                   MOVEL     'EIRE'        I95CAP                         *CAP
144500170120     c                   endif
144600170120     c
144700170120     C                   MOVEL     v1cLOD        I95LOC                         *LOCALITA'
144800170120     C                   Z-ADD     dspv1c        I95DAT                         *DATA RIFERIMENTO
144900170120     C                   MOVEL     v1cTPO        I95TPO                         *TIPO PORTO
145000170120     C                   MOVEL     v1cFFD        I95FFD                         *FERMO DEPOSITO
145100170120     C                   Z-ADD     v1cPKB        I95LKG                         *PESO
145200170120     C                   Z-ADD     v1cVLF        I95LMC                         *VOLUME
145300170120     C                   MOVEL     v1cTSP        I95TSP                         *TIPO SERVIZIO
145400170120     C* RECUPERA TERMINAL
145500170120     C                   CLEAR                   fnlv55ds
145600170120     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
145700170120     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
145800170120     C                   movel     V1CLNP        D55LIN                         *LINEA
145900170120     C                   Z-ADD     dspv1c        D55DRF                         *DATA RIFERIMENTO
146000170120     C                   CALL      'FNLV55R'
146100170120     C                   PARM                    fnlv55ds
146200170120     C                   Z-ADD     D55TFP        i95TFP                         *TERMINAL PARTENZA
146300170120     C                   MOVEL     'S'           I95FRE                         *CONTROLLA ECCEZIONI
146400170120IF  1C     v1cCAS        IFGT      *ZEROS
146500170120     C                   MOVEL     'S'           I95FCA                         *SI C/ASSEGNO
146600170120X   1C                   ELSE
146700170120     C                   MOVEL     'N'           I95FCA                         *NO C/ASSEGNO
146800170120E   1C                   ENDIF
146900170120     C                   IF        v1cNTW = 'DPD'
147000170120     C                   MOVEL     'D'           I95FI1
147100170120     C                   ENDIF
147200170120     C                   IF        v1cNTW = 'FED'
147300170120     C                   MOVEL     'S'           I95FI1
147400170120     C                   ENDIF
147500170120     c*
147600170120     c                   movel     v1ctc1        i95fi2
147700170120     c                   move      v1ctc2        i95fi2
147800170120     C* ERRORE AFFIDABILITA = 0
147900170120     C                   MOVEL     ' '           I13AF0
148000170120     C                   CALL      'FNLV13R'
148100170120     C                   PARM                    KPJBA
148200170120     C                   PARM                    fnlv13ds
148300170120     C                   PARM                    DSSI95
148400170120     C                   PARM      'S'           flgbac            1
148500170120     c
148600170120     C                   ENDSR
148700970704     C*------------------------------------------------------------------------*
148800970704     C* CHKLOC - CONTROLLA LA LOCALITA'
148900970704     C*------------------------------------------------------------------------*
149000970704     C     CHKLOC        BEGSR
149100970704     C*
149200970704     C     *LIKE         DEFINE    V1CLOD        WLOC
149300970704     C*
149400970704     C* RICHIESTO ELENCO
149500970704     C                   SETOFF                                           99
149600970704     C     '?'           SCAN      WLOC                                   99
149700970704IF  1C     *IN99         IFEQ      *ON
149800970704     C                   CLEAR                   DSSI00
149900970704     C                   MOVEL     'L01'         D00OP0
150000970704     C                   MOVEL     WNAR          D00NAR
150100970704     C                   MOVEL     WCAP          D00CAP
150200970704     C                   Z-ADD     DSPV1C        D00DRI
150300970704     C                   CALL      'TISI30R'
150400970704     C                   PARM                    KPJBA
150500970704     C                   PARM                    DSSI00
150600970704IF  2C     D00ERR        IFNE      '1'
150700970704     C                   MOVEL     D00NAR        WNAR                           *NAZIONE
150800970704     C                   MOVEL     D00CAP        WCAP                           *CAP
150900970704     C                   MOVEL     D00LOC        WLOC                           *LOCALITA'
151000970704E   2C                   ENDIF
151100970704     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
151200970704E   1C                   ENDIF
151300970704     C*
151400970704     C                   ENDSR
151500970704     C*------------------------------------------------------------------------*
151600970704     C* CHKCLP - CONTROLLA CAP + LOCALITA' + PROVINCIA
151700970704     C*------------------------------------------------------------------------*
151800970704     C     CHKCLP        BEGSR
151900970704     C*
152000970704     C* CONTROLLA ESISTENZA CODICE
152100970704     C                   CLEAR                   DSSI95
152200970704     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
152300970704     C                   MOVEL     '3'           I95TCN                         *CONTROLLO
152400970704     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
152500120802     c                   if        wnar='IRL'
152600120802     C                   MOVEL     'EIRE'        I95CAP                         *CAP
152700120802     c                   else
152800120802     C                   MOVEL     WCAP          I95CAP                         *CAP
152900120802     c                   endif
153000970704     C                   MOVEL     WLOC          I95LOC                         *LOCALITA'
153100970704     C                   Z-ADD     DSPV1C        I95DAT                         *DATA RIFERIMENTO
153200020225     C                   IF        V1CNTW = 'FED'
153300020225     C                   MOVEL     'S'           I95FI1
153400020225     C                   ENDIF
153500090615     c* C/ASS
153600090615     C     v1ccas        IFgt      0
153700090615     C                   MOVEL     'S'           I95FCA
153800090615     C                   ENDIF
153900090615     c* Tipo bolla
154000090615     C                   MOVEL     v1ctpo        I95TPO
154100090615     C* ERRORE AFFIDABILITA = 0
154200090615     C                   MOVEL     ' '           I13AF0
154300090615     C                   CALL      'FNLV13R'
154400090615     C                   PARM                    KPJBA
154500090615     C                   PARM                    fnlv13ds
154600090615     C                   PARM                    DSSI95
154700090615     C                   PARM      ' '           flgbac            1
154800090615     C                   PARM                    tisi97ds
154900090615     c
155000090615     c* Errore per il network
155100090616    0c                   if        i97ntw<>*blanks
155200090617     c                   movel     o97flo        dcppflo
155300090617     c
155400090617     c                   select
155500090617    1c                   when      forzacaputi<>' '
155600090617     c
155700090617    1c                   when      o13err<>*blanks
155800090615     C                   MOVEL     '1'           $ERR
155900090618    2c                   if        §cppall=' 'and
156000090618     c                             §cpppas<>' ' and §cppcas='S'
156100090618     c                   eval      o13msg=msg(44)
156200090618    2c                   endif
156300090618     c
156400090617   x1c                   other
156500090617    2c                   if        §cpppas='O' and v1ctpo='A'
156600090616     C                   MOVEL     '1'           $ERR
156700090617     c                   eval      o13msg=msg(45)
156800090618     c                   else
156900090618     c                   eval       o13msg=%trim(o13msg)+':ENTR forza'
157000090616    2c                   endif
157100090617    1c                   endsl
157200090616     c
157300090615   x0c                   else
157400090615     c
157500090615     c* Errore sulla località
157600970704IF  1C     O95ERR        IFNE      *BLANKS
157700120802     c
157800090615IF   C     o13err        orNE      *BLANKS
157900090629     c     o13rop        andeq     *blanks
158000120802
158100970714     C     O95LIA        OREQ      '1'
158200120802     c
158300120802     C     WLOC          ORNE      *BLANKS
158400120802     C     Wnar          andeq     *BLANKS
158500970714     C     O95LID        ANDLT     '3'
158600970704     C                   MOVEL     '1'           $ERR
158700970704E   1C                   ENDIF
158800090615E   0C                   ENDIF
158900970704     C*
159000970704     C                   ENDSR
159100000000     C*------------------------------------------------------------------------*
159200000000     C* CHKTSP - CONTROLLA IL TIPO SERVIZIO
159300000000     C*------------------------------------------------------------------------*
159400000000     C     CHKTSP        BEGSR
159500000000     C*
159600000000     C     *LIKE         DEFINE    V1CTSP        WTSP
159700000000     C     *LIKE         DEFINE    V1DTSP        WDTSP
159800000000     C*
159900970704     C* RICHIESTO ELENCO
160000000000     C     '?'           SCAN      WTSP                                   99
160100000000IF  1C     *IN99         IFEQ      *ON
160200020919     C                   MOVEL     1             §KUT              1 0
160300000000     C                   MOVEL     '5E'          §TAB              2
160400000000     C                   MOVEL     *BLANKS       §KEY              8
160500960926     C                   MOVEL     *BLANKS       §DES             30
160600000000     C                   CALL      'X§TABER'
160700000000     C                   PARM                    §KUT
160800000000     C                   PARM                    §TAB
160900000000     C                   PARM                    §KEY
161000960926     C                   PARM                    §DES
161100000000     C                   MOVEL     §KEY          WTSP
161200960926     C                   MOVEL     §DES          WDTSP
161300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
161400970704E   1C                   ENDIF
161500000000     C*
161600970704     C* CONTROLLA ESISTENZA CODICE
161700020919     C                   MOVEL     1             KTBKUT
161800000000     C                   MOVEL     '5E'          KTBCOD
161900000000     C                   MOVEL     *BLANKS       KTBKEY
162000000000     C                   MOVEL     WTSP          KTBKEY
162100000000     C     KEYTAB        CHAIN     TABEL00F                           99
162200970704IF  1C     *IN99         IFEQ      *OFF
162300000000     C     TBLFLG        ANDEQ     *BLANKS
162400000000     C                   MOVEL     TBLUNI        DS5E
162500000000     C                   MOVEL     §5EDES        WDTSP
162600970704X   1C                   ELSE
162700000000     C                   MOVEL     '1'           $ERR
162800000000     C                   MOVEL     *ALL'*'       WDTSP
162900000000E   1C                   ENDIF
163000000000     C*
163100960703     C                   ENDSR
163200960926     C*------------------------------------------------------------------------*
163300960926     C* CHKDIV - CONTROLLA LA DIVISA
163400960926     C*------------------------------------------------------------------------*
163500960926     C     CHKDIV        BEGSR
163600960926     C*
163700960926     C     *LIKE         DEFINE    V1CVCA        WDIV
163800960926     C     *LIKE         DEFINE    §CVFDC        WFDC
163900010823     C     *LIKE         DEFINE    §CVDVA        WDVA
164000010823     C     *LIKE         DEFINE    §CWDLC        WDLC
164100960926     C*
164200970704     C* RICHIESTA ELENCO
164300960926     C     '?'           SCAN      WDIV                                   99
164400960926IF  1C     *IN99         IFEQ      *ON
164500020919     C                   Z-ADD     1             §KUT
164600960926     C                   MOVEL     'CV'          §COD
164700960926     C                   MOVEL     *BLANKS       §KEY
164800960926     C                   MOVEL     *BLANKS       §DES
164900960926     C                   CALL      'X§TABER'
165000960926     C                   PARM                    §KUT              1 0
165100960926     C                   PARM                    §COD              2
165200960926     C                   PARM                    §KEY              8
165300960926     C                   PARM                    §DES             30
165400960926     C                   MOVEL     §KEY          WDIV
165500960926     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
165600960926E   1C                   ENDIF
165700960926     C*
165800960926     C* CONTROLLA ESISTENZA CODICE
165900020919     C                   Z-ADD     1             KTBKUT
166000960926     C                   MOVEL     'CV'          KTBCOD
166100960926     C                   MOVEL(P)  WDIV          KTBKEY
166200960926     C     KEYTAB        CHAIN     TABEL00F                           99
166300960926IF  1C     *IN99         IFEQ      *ON
166400960926     C     TBLFLG        OREQ      '*'
166500960926     C                   MOVEL     '1'           $ERR
166600960926X   1C                   ELSE
166700960926     C                   MOVEL     TBLUNI        DSCV
166800960926     C                   MOVEL     §CVFDC        WFDC                           *DIVISA CON IMP.DEC.
166900010823     C                   MOVEL     §CVDVA        WDVA                           *S=SI DIVISA ALTERNA
167000010823     C                   ENDIF
167100010823     C*
167200010823     C* REPERISCO GIA' QUI L'ESTENSIONE DELLA TABELLA CV
167300020919     C                   Z-ADD     1             KTBKUT
167400010823     C                   MOVEL     'CW'          KTBCOD
167500010823     C                   MOVEL(P)  WDIV          KTBKEY
167600010823     C     KEYTAB        CHAIN     TABEL00F                           99
167700010823IF  1C     *IN99         IFEQ      *ON
167800010823     C     TBLFLG        OREQ      '*'
167900010823     C                   MOVEL     '1'           $ERR
168000010823X   1C                   ELSE
168100010823     C                   MOVEL     TBLUNI        DSCW
168200010823     C                   Z-ADD     §CWDLC        WDLC                           *DATA LIMITE CONTABI
168300010823     C                   ENDIF
168400960926     C*
168500960926     C                   ENDSR
168600960926     C*------------------------------------------------------------------------*
168700960926     C* CHKTIC - CONTROLLA IL TIPO INCASSO C/ASSEGNO
168800960926     C*------------------------------------------------------------------------*
168900960926     C     CHKTIC        BEGSR
169000960926     C*
169100960926     C     *LIKE         DEFINE    V1CTIC        WTIC
169200960926     C     *LIKE         DEFINE    §1AUDE        WUDE
169300010823     C     *LIKE         DEFINE    §1ACDL        WCDL
169400960926     C*
169500970704     C* RICHIESTA ELENCO
169600960926     C     '?'           SCAN      WTIC                                   99
169700960926IF  1C     *IN99         IFEQ      *ON
169800020919     C                   Z-ADD     1             §KUT
169900960926     C                   MOVEL     '1A'          §COD
170000960926     C                   MOVEL     *BLANKS       §KEY
170100960926     C                   MOVEL     *BLANKS       §DES
170200960926     C                   CALL      'X§TABER'
170300960926     C                   PARM                    §KUT              1 0
170400960926     C                   PARM                    §COD              2
170500960926     C                   PARM                    §KEY              8
170600960926     C                   PARM                    §DES             30
170700960926     C                   MOVEL     §KEY          WTIC
170800960926     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
170900960926E   1C                   ENDIF
171000960926     C*
171100010823     C* CONTROLLA ESISTENZA CODICE
171200020919     C                   Z-ADD     1             KTBKUT
171300960926     C                   MOVEL     '1A'          KTBCOD
171400960926     C                   MOVEL(P)  WTIC          KTBKEY
171500960926     C     KEYTAB        CHAIN     TABEL00F                           99
171600960926IF  1C     *IN99         IFEQ      *ON
171700960926     C     TBLFLG        OREQ      '*'
171800960926     C                   MOVEL     '1'           $ERR
171900960926X   1C                   ELSE
172000960926     C                   MOVEL     TBLUNI        DS1A
172100960926     C                   MOVEL     §1AUDE        WUDE                           *ABILITATO X DIV EST
172200010823     C                   MOVEL     §1ACDL        WCDL                           *FLAG DATA LIMITE CO
172300960926E   1C                   ENDIF
172400010823     C*
172500010823     C* CONTROLLO INSERIBILITA' TIPO INCASSO RISPETTO ALLA DIVISA
172600010823     C     $ERR          IFEQ      '0'
172700010823     C     WDVA          IFNE      'S'
172800010823     C     WUDE          ANDNE     'S'
172900010823     C                   MOVEL     '1'           $ERR
173000010823     C                   MOVEL     MSG(26)       $MSG
173100010823     C                   EXSR      CARMSG
173200010823     C                   SETON                                        7374
173300010823     C                   ELSE
173400010823     C     WCDL          IFEQ      'S'
173500010823     C     DSPV1C        IFGT      WDLC
173600010823     C                   MOVEL     '1'           $ERR
173700010823     C                   MOVEL     MSG(30)       $MSG
173800010823     C                   EXSR      CARMSG
173900010823     C                   SETON                                        7374
174000010823     C                   ENDIF
174100010823     C                   ENDIF
174200010823     C                   ENDIF
174300010823     C                   ENDIF
174400960926     C*
174500960926     C                   ENDSR
174600960614     C*------------------------------------------------------------------------*
174700960614     C* CHKKSC - CONTROLLA CODICE CLIENTE
174800960614     C*------------------------------------------------------------------------*
174900960614     C     CHKKSC        BEGSR
175000960614     C*
175100960614     C     *LIKE         DEFINE    V1CKSC        WKSC
175200960614     C     *LIKE         DEFINE    V1DKSC        WDKSC
175300960614     C*
175400970704     C* RICHIESTO ELENCO
175500960614     C     '?'           SCAN      WKSC                                   28
175600960614IF  1C     *IN28         IFEQ      *ON
175700960614     C                   CLEAR                   DSXC
175800020919     C                   Z-ADD     DUTKCI        DXCCAP                         *CAPOCONTO
175900110616     c                   eval      dxcaut = otaa1cabi
176000960614     C                   CALL      'XCLIR'
176100960614     C                   PARM                    DSXC
176200960614     C* SCOMPATTA
176300960614     C                   MOVEA     DXCKSC        KSA                            *DS INPUT -> SCH ALF
176400960614DO  2C     1             DO        30            J                 4 0
176500960614     C                   MOVE      KSA(J)        DSKSA                          *SCH ALF -> DS ALF
176600960614     C                   Z-ADD     DSKSN         KSC(J)                         *DS NUM -> SCH NUM
176700960614E   2C                   ENDDO
176800960614     C                   MOVEL     KSC(1)        WKSC
176900960614     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
177000960614E   1C                   ENDIF
177100960614     C*
177200970704     C* CONTROLLA ESISTENZA CODICE
177300960617IF  1C     WKSC          IFLE      *ZEROS                                       *SOLO VALORE NUMERIC
177400960614     C                   MOVEL     '1'           $ERR
177500960614X   1C                   ELSE
177600020919     C                   Z-ADD     1             KACKUT
177700020919     C                   Z-ADD     DUTKCI        KACKCC
177800960614     C                   MOVEL     WKSC          KACKSC
177900960614     C     KEYACO        CHAIN     CNACO00F                           99
178000960614IF  2C     *IN99         IFEQ      *ON                                          *CLIENTE INESISTENTE
178100960614     C     ACOFLG        ORNE      *BLANKS                                      *ANNULLATO
178200960614     C                   MOVEL     '1'           $ERR
178300960618X   2C                   ELSE                                                   *CLIENTE VALIDO
178400960618     C*
178500960618     C* CLIENTE DI FILIALE IN GESTIONE
178600020925     C                   MOVEL     WKSC          W003              3            *FIL+XXXX
178700020925IF  3C     W003          IFNE      '888'
178800100406     C**                 SETOFF                                           28
178900100406     C**   W003          LOOKUP    L1                                     28
179000100406IF  4C**   *IN28         IFEQ      *OFF
179100100406     C**                 MOVEL     '1'           $ERR
179200100406E   3C**                 ENDIF
179300100406     c                   clear                   tntaa1ds
179400100406     c                   eval      itaa1caut='§utegtc'
179500100406     c                   eval      itaa1ksc=%int(wksc)
179600100406     c                   clear                   kpjbu
179700100406     c                   movel     tntaa1ds      kpjbu
179800100406     c                   call      'TNTAA1C'
179900100406     c                   parm                    kpjba
180000100406     c                   movel     kpjbu         tntaa1ds
180100100406     c
180200100406    4c                   if        otaa1fabi='N'
180300100406     C                   MOVEL     '1'           $ERR
180400100406E   4C                   ENDIF
180500100406E   3C                   ENDIF
180600960618     C*
180700960614E   2C                   ENDIF
180800960614E   1C                   ENDIF
180900960614     C*
181000960614     C                   ENDSR
181100110623     C*------------------------------------------------------------------------*
181200110623     C* CHKINL - CONTROLLA L'INOLTRO
181300110623     C*------------------------------------------------------------------------*
181400110623     C     CHKINL        BEGSR
181500110623     C*
181600110623     C     *LIKE         DEFINE    V1cISO        WINL
181700110623     C     *LIKE         DEFINE    V1DISO        WDINL
181800110623     C*
181900110623     C* RICHIESTO ELENCO
182000110623     C                   SETOFF                                           28
182100110623     C     '?'           SCAN      WINL                                   28
182200110623IF  1C     *IN28         IFEQ      *ON
182300110623     C                   Z-ADD     1             §KUT              1 0
182400110623     C                   MOVEL     '9A'          §COD              2
182500110623     C                   MOVEL     *BLANKS       §KEY              8
182600110623     C                   MOVEL     *BLANKS       §DES             30
182700110623     C                   CALL      'X§TABER'
182800110623     C                   PARM                    §KUT
182900110623     C                   PARM                    §COD
183000110623     C                   PARM                    §KEY
183100110623     C                   PARM                    §DES
183200110623     C                   MOVEL     §KEY          WINL
183300110623     C                   MOVEL     §DES          WDINL
183400110623     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
183500110623E   1C                   ENDIF
183600110623     C*
183700110623     C* CONTROLLA ESISTENZA CODICE
183800110623     C                   MOVEL     1             KTBKUT
183900110623     C                   MOVEL     '9A'          KTBCOD
184000110623     C                   MOVEL(P)  WINL          KTBKEY
184100110623     C     KEYTAB        CHAIN     TABEL00F                           99
184200110623IF  1C     *IN99         IFEQ      *OFF
184300110623     C     TBLFLG        ANDEQ     *BLANKS
184400110623     C                   MOVEL     TBLUNI        DS9A
184500110623     C                   MOVEL     §9ADES        WDINL
184600110623X   1C                   ELSE
184700110623     C                   MOVEL     '1'           $ERR
184800110623E   1C                   ENDIF
184900110627     c*
185000110627     c                   if        $err='0' and §9autiop='N'
185100110627     C                   MOVEL     '1'           $ERR
185200110627E   1C                   ENDIF
185300110623     C*
185400110623     C                   ENDSR
185500960614     C*------------------------------------------------------------------------*
185600960614     C* CHKCTR - CONTROLLA TARIFFA CLIENTE
185700960614     C*------------------------------------------------------------------------*
185800960614     C     CHKCTR        BEGSR
185900960614     C*
186000960614     C     *LIKE         DEFINE    V1CCTR        WCTR
186100960617     C                   MOVEL     '0'           $CTR              1            *VALIDITA' TARIFFA
186200090429     C* RICERCA
186300090429     C     '?'           SCAN      v1cctr                                 30
186400090429     C*
186500090429    0C     *IN30         IFEQ      *ON
186600090429     C                   CLEAR                   PARAM4
186700090429     C                   MOVEL     v1cksc        PA4COD
186800090429     C                   Z-ADD     dutkci        PA4CCC
186900090429     c                   movel     '2'           pa4vpr
187000090429     c                   move      dspv1c        pa4dtr
187100090429     C                   MOVEL     PARAM4        KPJBU
187200090429     C                   CALL      'TNTA36R'
187300090429     C                   PARM                    KPJBA
187400090429     c* per riemettere videata
187500090429     C                   MOVEL     '1'           $ERR
187600090429     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
187700090429     C                   MOVEL     KPJBU         PARAM4
187800090429     c                   seton                                        71
187900090429     c
188000090429     C* SE TROVATO METTO QUELLO SCELTO
188100090429    1C     PA4CTR        IFNE      *BLANKS
188200090429     C                   MOVE      PA4CTR        v1cctr
188300090429     C                   MOVE      PA4dcv        v1dctr
188400090429   X1C                   ELSE
188500090429     c* Se non scelto lo abblanco
188600090429     C                   MOVEL     *blanks       v1cctr
188700090429     C                   MOVEL     *blanks       v1dctr
188800090429    1C                   END
188900090429     C*
189000090429   x0c                   else
189100960617     C*
189200970704     C* CONTROLLA ESISTENZA CODICE
189300960618IF  1C     WCTR          IFLT      *ZEROS                                       *SOLO VALORE NUMERIC
189400960617     C                   MOVEL     '1'           $ERR
189500090429     C                   MOVEL     MSG(21)       $MSG
189600090429     C                   EXSR      CARMSG
189700090429     C                   SETON                                        71
189800090429     c
189900960617X   1C                   ELSE
190000981214     C                   MOVEL     V1CKSC        KAMKSC
190100981214     C                   MOVEL     WCTR          KAMCTR
190200981214     C                   Z-ADD     999           KAMPRG
190300960617     C     KEYTAM        SETLL     TNTAM01L
190400960617     C     KE2TAM        READPE    TNTAM01L                               99
190500960617DO  2C     *IN99         DOWEQ     *OFF
190600960617IF  3C     TAMATB        IFEQ      *BLANKS
190700130108     c     tamddt        andle     dspv1c
190800130108     c
190900960703     C*---       TAMDST    ANDGEDSPV1C
191000021106     C*
191100021107     C* SE VUOTO IMPOSTO IL CAMPO A VIDEO RELATIVO AL DDT CON QUANTO PRESENTE SULLA TARIFFA
191200021107     C                   IF        V1CDDT = *BLANKS
191300021106     C                   MOVEL     TAMBAM        V1CDDT
191400021107     C                   ENDIF
191500960617     C                   MOVEL     '1'           $CTR
191600090429     c                   movel     tamdcv        v1dctr
191700960617     C                   SETON                                            99
191800960617E   3C                   ENDIF
191900960617     C  N99KE2TAM        READPE    TNTAM01L                               99
192000960617E   2C                   ENDDO
192100960617IF  2C     $CTR          IFEQ      '0'
192200960617     C                   MOVEL     '1'           $ERR
192300090429     C                   MOVEL     MSG(21)       $MSG
192400090429     C                   EXSR      CARMSG
192500090429     C                   SETON                                        71
192600090429   x2c                   else
192700090429     c
192800090429    3c                   if        (v1cksc<>savksc)  or (v1cctr<>savctr)
192900090429     c                   movel     v1cksc        savksc
193000090429     c                   movel     v1cctr        savctr
193100090429     c                   clear                   forzaBLO
193200090429     c                   clear                   forzaPA
193300090506     c                   clear                   forzaIE
193400090429    3c                   endif
193500090429     c
193600090429     c* Altrimenti controllo se tariffa corretta
193700090615    3c  n71              if        tambap='B'   and forzaBLO=' '
193800090429     C                   MOVEL     '1'           forzaBLO
193900090429     C                   MOVEL     '1'           $ERR
194000090429     C                   MOVEL     MSG(37)       $MSG
194100090429     C                   EXSR      CARMSG
194200090429     C                   SETON                                        71
194300090429    3c                   endif
194400090615    3c  n71              if        tambap='A' and v1ctpo='F' and forzaPA=' '
194500090429     C                   MOVEL     '1'           forzaPA
194600090429     C                   MOVEL     '1'           $ERR
194700090429     C                   MOVEL     MSG(39)       $MSG
194800090429     C                   EXSR      CARMSG
194900090429     C                   SETON                                        71
195000090429    3c                   endif
195100090615    3c  n71              if        tambap='P' and v1ctpo='A'  and forzaPA=' '
195200090429     C                   MOVEL     '1'           forzaPA
195300090429     C                   MOVEL     '1'           $ERR
195400090429     C                   MOVEL     MSG(38)       $MSG
195500090429     C                   EXSR      CARMSG
195600090429     C                   SETON                                        71
195700090429    3c                   endif
195800090506     c* Tariffa italia/estero
195900090506     c                   movel     tamflo        dsta01
196000090615     c  n71              if        §tadpd='S' and  v1cntw<>'DPD' and forzaIE=' '
196100120509     c                             and §ogntw<>'DPD'
196200090506     C                   MOVEL     '1'           forzaIE
196300090506     C                   MOVEL     '1'           $ERR
196400090506     C                   MOVEL     MSG(40)       $MSG
196500090506     C                   EXSR      CARMSG
196600090506     C                   SETON                                        71
196700090506    3c                   endif
196800090615     c  n71              if        §taFED='S' and  v1cntw<>'FED' and forzaIE=' '
196900090506     C                   MOVEL     '1'           forzaIE
197000090506     C                   MOVEL     '1'           $ERR
197100090506     C                   MOVEL     MSG(40)       $MSG
197200090506     C                   EXSR      CARMSG
197300090506     C                   SETON                                        71
197400090506    3c                   endif
197500090615     c  n71              if        §taFED=' ' and v1cntw<>'EEX' and forzaIE=' '
197600120509     c                             and tamfie='E' and §ogntw<>'EEX'
197700090506     C                   MOVEL     '1'           forzaIE
197800090506     C                   MOVEL     '1'           $ERR
197900090506     C                   MOVEL     MSG(40)       $MSG
198000090506     C                   EXSR      CARMSG
198100090506     C                   SETON                                        71
198200090506    3c                   endif
198300120509     c  n71              if        §tadpd=' '  and forzaIE=' '
198400120509     c                             and tamfie='I' and (v1cntw<>'   ' or
198500120509     c                              §ogntw='EEX' or §ogntw='FED' or
198600120509     c                              §ogntw='DPD')
198700090506     C                   MOVEL     '1'           forzaIE
198800090506     C                   MOVEL     '1'           $ERR
198900090506     C                   MOVEL     MSG(40)       $MSG
199000090506     C                   EXSR      CARMSG
199100090506     C                   SETON                                        71
199200090506    3c                   endif
199300090429     c
199400960617E   2C                   ENDIF
199500960617E   1C                   ENDIF
199600090429E   0C                   ENDIF
199700960614     C*
199800960614     C                   ENDSR
199900000000     C*------------------------------------------------------------------------*
200000000000     C* IMPVID  - IMPOSTAZIONI INIZIALI VIDEO
200100000000     C*------------------------------------------------------------------------*
200200000000     C     IMPVID        BEGSR
200300960528     C***
200400960528     C* SIMULAZIONE DELIVERY
200500960528     C***
200600000103     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
200700000103     C                   MOVE      N14           V1CDSP                         *DATA SPEDIZIONE
200800020919     C                   MOVEL     DUTPOU        V1CLNP                         *FILIALE PARTENZA
200900960704     C                   MOVEL     *BLANKS       V1CNAR                         *NAZIONE
201000960704     C                   MOVEL     'C'           V1CTSP                         *TIPO SERVIZIO
201100971126     C                   MOVEL     'F'           V1CTPO                         *TIPO PORTO
201200980116     C                   MOVEL     'N'           V1CFFD                         *FERMO DEPOSITO
201300000919     C                   MOVEL     'N'           V1CFDN                         *MERCE A MAGAZZINO
201400110817     C                   MOVEL     'N'           V1Cemd                         *email al destinat.
201500050413     c                   clear                   v1ctc1
201600050413     c                   clear                   v1ctc2
201700960528     C***
201800960528     C* SIMULAZIONE TASSAZIONE
201900960528     C***
202000960729     C                   Z-ADD     1             V1CNCL                         *COLLI
202100960528     C                   Z-ADD     1             V1CPKB                         *PESO
202200020313     C                   Z-ADD     0,005         V1CVLF                         *VOLUME
202300960703     C                   MOVEL     *ZEROS        V1CKSC                         *CODICE CLIENTE
202400960703     C                   MOVEL     *BLANKS       V1DKSC                         *DESCRIZIONE CLIENTE
202500960703     C                   MOVEL     *ZEROS        V1CCTR                         *CODICE TARIFFA
202600960926     C                   Z-ADD     *ZEROS        V1CCAS                         *IMPORTO C/ASSEGNO
202700960926     C                   MOVEL     *BLANKS       V1CVCA                         *VALUTA  C/ASSEGNO
202800960926     C                   MOVEL     *BLANKS       V1CTIC                         *INCASSO C/ASSEGNO
202900960926     C                   Z-ADD     *ZEROS        V1CIAS                         *IMPORTO ASSICURAZ.N
203000960926     C                   MOVEL     *BLANKS       V1CVAS                         *VALUTA  ASSICURAZ.N
203100020813     C                   Z-ADD     *ZEROS        V1CSCO                         *% DI SCONTO
203200050415     C                   Z-ADD     *ZEROS        V1Cqft                         *% DI SCONTO
203300090616     c                   clear                   forzaCAPuti
203400090616     c                   clear                   forzaIE
203500090616     c                   clear                   forzaBLO
203600090616     c                   clear                   forzaPA
203700120608     c                   clear                   savpkb
203800120608     c                   clear                   savtsp
203900160302     c                   clear                   savt_ntw
204000960704     C*
204100001121     C* RICHIAMO LA ROUTINE SEGUENTE PER GESTIRE LE CHIAMATE DALLA PROCEDURA "ORM"
204200001115     C                   EXSR      TSTPAR
204300001115     C*
204400000000     C                   ENDSR
204500001115     C*------------------------------------------------------------------------*
204600001115     C* TSTPAR - TEST DEI PARAMETRI IN INGRESSO PER VALORIZZAZIONE VIDEO DA CHIAMANTE
204700001115     C*------------------------------------------------------------------------*
204800001115     C     TSTPAR        BEGSR
204900001115     C*
205000001115     C* VERIFICA PARAMETRI
205100001116     C                   MOVEL     KPJBU         DSSI92
205200001116     C     D92OP0        IFEQ      'ORM'
205300001115     C     D92LNP        IFGT      *ZEROS
205400001115     C                   MOVEL     D92LNP        V1CLNP
205500001115     C                   ENDIF
205600001115     C     D92NAR        IFGT      *BLANKS
205700001115     C                   MOVEL     D92NAR        V1CNAR
205800001115     C                   ENDIF
205900001115     C     D92CAD        IFGT      *BLANKS
206000001115     C                   MOVEL     D92CAD        V1CCAD
206100001115     C                   ENDIF
206200001115     C     D92LOD        IFGT      *BLANKS
206300001115     C                   MOVEL     D92LOD        V1CLOD
206400001115     C                   ENDIF
206500001115     C     D92FFD        IFGT      *BLANKS
206600001115     C                   MOVEL     D92FFD        V1CFFD
206700001115     C                   ENDIF
206800001115     C     D92NCL        IFGT      *ZEROS
206900001115     C                   Z-ADD     D92NCL        V1CNCL
207000001115     C                   ENDIF
207100001115     C     D92PKB        IFGT      *ZEROS
207200001115     C                   Z-ADD     D92PKB        V1CPKB
207300001115     C                   ENDIF
207400001115     C     D92VLF        IFGT      *ZEROS
207500001115     C                   Z-ADD     D92VLF        V1CVLF
207600001115     C                   ENDIF
207700001115     C     D92KSC        IFGT      *ZEROS
207800001115     C                   MOVEL     D92KSC        V1CKSC
207900001115     C                   ENDIF
208000001115     C     D92CTR        IFGT      *ZEROS
208100001115     C                   MOVEL     D92CTR        V1CCTR
208200001115     C                   ENDIF
208300001115     C                   ENDIF
208400001115     C*
208500001115     C                   ENDSR
208600020225     C*------------------------------------------------------------------------*
208700020225     C* CARTAR - CARICA LE TARIFFE DI CARTELLO
208800020225     C*------------------------------------------------------------------------*
208900020225     C     CARTAR        BEGSR
209000020225     C*---
209100020225     C* TARIFFA DI CARTELLO
209200020225     C*---
209300160302     C                   CLEAR                   TRULc7DS
209400160302     C                   EVAL      Ic7TLA = 'L'
209500160302     C                   EVAL      Ic7tNTW = V1CNTW
209600160302     c                   eval      ic7pkg=v1cpkb
209700160302     C                   CALL      'TRULC7R'
209800160302     C                   PARM                    TRULc7DS
209900020225     C*
210000020225     C                   ENDSR
210100020925     C*--------------------------------------------------------------------------------------------*
210200020925     C* REPERISCE DATI UTENTE
210300020925     C*--------------------------------------------------------------------------------------------*
210400020925     C     REPDATIUTE    BEGSR
210500020925     C*
210600020925     C* REimposta le variabili di lavoro
210700020925     C                   CLEAR                   TIBS34DS
210800020925     C                   CLEAR                   AZUTEDS
210900020925     C                   CLEAR                   DDATIUTE
211000100406     C**                 CLEAR                   WABI              2
211100100406     C**                 CLEAR                   DLAT
211200020925     C*
211300020925     C* Reperisce i dati del profilo in ingresso
211400020925     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
211500020925     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
211600020925     C                   IN(E)     *DTAARA
211700020925     C                   IF        %Error                                       *NO errori
211800020925     C                   EVAL      I34TLA = 'L'
211900020925     C                   CALL      'TIBS34R'
212000020925     C                   PARM                    TIBS34DS
212100020925     C                   IN        *DTAARA
212200020925     C                   ENDIF
212300100406     c
212400100406     c* Richiamo solo per avere codice di abilitazione
212500100406     c                   clear                   tntaa1ds
212600100406     c                   eval      itaa1caut='§utegtc'
212700100406     c                   clear                   kpjbu
212800100406     c                   movel     tntaa1ds      kpjbu
212900100406     c                   call      'TNTAA1C'
213000100406     c                   parm                    kpjba
213100100406     c                   movel     kpjbu         tntaa1ds
213200100406     c
213300100406     c* Utente non abilitato alla gestione tariffe
213400100406     c                   if        otaa1fabi='N'
213500100406     C                   EVAL      $ERR  = '1'
213600100406     C                   EVAL      $FINE = '1'
213700100406     C                   ENDIF
213800020925     C*
213900020925     C* Se ci sono degli errori sull'abilitazione utente, esce
214000100406     C**                 SELECT
214100100406     C**                 WHEN      DUTERR = 'E'
214200100406     C**                 EVAL      $ERR  = '1'
214300100406     C**                 EVAL      $FINE = '1'
214400020925     C*
214500020925     C* NON siamo in sede (filiale remota o filiale locale)
214600020925     C* --> se 1° livello, abilitazioni al terminal
214700020925     C*     se 2° livello, abilitazioni al punto operativo
214800100406     C**                 WHEN      UTEAUT = *blanks
214900100406     C**                 IF        DUTLPO = '1'                                 *1° LIVELLO
215000100406     C**                 EVAL      WABI   = 'TP'
215100100406     C**                 ENDIF
215200100406     C**                 IF        DUTLPO = '2'                                 *2° LIVELLO
215300100406     C**                 EVAL      WABI   = 'PO'
215400100406     C**                 ENDIF
215500020925     C*
215600020925     C* siamo in sede
215700020925     C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
215800020925     C*     carica le abilitazioni proprie del profilo
215900100406     C**                 OTHER
216000100406     C**                 MOVEL     UTEFAF        DUTE01
216100100406     C**                 IF        §UTEIST <> *blanks                           *esiste specifica
216200100406     C**                 EVAL      WABI = §UTEIST
216300100406     C**                 ELSE                                                   *NON esiste specific
216400100406     C**                 EVAL      WABI = UTEAUT
216500100406     C**                 ENDIF
216600100406     C**                 ENDSL
216700020925     C*
216800020925     C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
216900100406     C**                 CLEAR                   TIBS02DS
217000100406     C*+                 MOVEL     'C'           T02MOD
217100100406     C**                 MOVEL     KNSIF         T02SIF
217200100406     C**                 MOVEL     'LAT'         T02COD
217300100406     C**                 MOVEL(P)  WABI          T02KE1
217400100406     C**                 EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
217500100406     C**                 CALL      'TIBS02R'
217600100406     C**                 PARM                    KPJBA
217700100406     C**                 PARM                    TIBS02DS
217800100406     c**
217900100406     C**                 EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
218000100406     C**                 IF        T02ERR = *blanks
218100100406     C**                 MOVEL     T02UNI        DLAT
218200100406     C**                 ENDIF
218300100406     C**                 IF        T02ERR <> *blanks OR
218400100406     C**                           §LATABI = 'S'
218500100406     C**                 EVAL      $ERR  = '1'
218600100406     C**                 EVAL      $FINE = '1'
218700100406     C**                 ENDIF
218800100406     C**
218900100406     C**Reperisce i P.O. gestibili dall'utente
219000100406     C**                 IF        $ERR <> '1'                                  *NO errori
219100100406     C**                 CLEAR                   TRUL31DS
219200100406     C**                 EVAL      I31ABI   = WABI
219300100406     C**                 EVAL      I31CDI   = DUTDIS
219400100406     C**                 EVAL      I31CAR   = DUTARE
219500100406     C**                 EVAL      I31CPO   = DUTPOU
219600100406     C**                 EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
219700100406     C**                 CALL      'TRUL31R'
219800100406     C**                 PARM                    KPJBA
219900100406     C**                 PARM                    TRUL31DS
220000100406     C**                 EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
220100100406if  2C**                 IF        O31POG > *zeros
220200100406     C**                 MOVEA     O31POG        L1
220300100406x   2C**                 ELSE
220400100406     C**                 EVAL      $ERR  = '1'
220500100406     C**                 EVAL      $FINE = '1'
220600100406e   2C**                 ENDIF
220700100406e   1C**                 ENDIF
220800020925     C*
220900020925     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
221000020925     C                   MOVEL(P)  RSUT          DSFIRS           20
221100020925     C*
221200020925     C                   ENDSR
221300000000     C*------------------------------------------------------------------------*
221400000000     C* *INZSR  - ROUTINE INIZIALE
221500000000     C*------------------------------------------------------------------------*
221600000000     C     *INZSR        BEGSR
221700000000     C*--------------------
221800000000     C* RICEVIMENTO PARAMETRI
221900000000     C*--------------------
222000000000     C     *ENTRY        PLIST
222100000000     C                   PARM                    KPJBA
222200000000     C*--------------------
222300000000     C* CAMPI RIFERITI AL DATA BASE
222400000000     C*--------------------
222500000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
222600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
222700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
222800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
222900960614     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNACO00F
223000960614     C     *LIKE         DEFINE    ACOKCC        KACKCC
223100960614     C     *LIKE         DEFINE    ACOKSC        KACKSC
223200981214     C     *LIKE         DEFINE    TAMKSC        KAMKSC                         *TNTAM00F
223300981214     C     *LIKE         DEFINE    TAMCTR        KAMCTR
223400981214     C     *LIKE         DEFINE    TAMPRG        KAMPRG
223500020925     C     *LIKE         DEFINE    KPJBU         DEPKPJBU                       *depositi
223600000000     C*--------------------
223700000000     C* CHIAVI DI LETTURA
223800000000     C*--------------------
223900000000     C* LETTURA AZORG01L - COMPLETA
224000000000     C     KEYORG        KLIST
224100000000     C                   KFLD                    KORFIL
224200000000     C* LETTURA TABEL00F - COMPLETA
224300000000     C     KEYTAB        KLIST
224400000000     C                   KFLD                    KTBKUT
224500000000     C                   KFLD                    KTBCOD
224600000000     C                   KFLD                    KTBKEY
224700960614     C* LETTURA CNACO00F - COMPLETA
224800960614     C     KEYACO        KLIST
224900960614     C                   KFLD                    KACKUT
225000960614     C                   KFLD                    KACKCC
225100960614     C                   KFLD                    KACKSC
225200960617     C* LETTURA TNTAM01L - COMPLETA
225300960617     C     KEYTAM        KLIST
225400981214     C                   KFLD                    KAMKSC
225500981214     C                   KFLD                    KAMCTR
225600981214     C                   KFLD                    KAMPRG
225700960617     C* LETTURA TNTAM01L - PARZIALE
225800960617     C     KE2TAM        KLIST
225900981214     C                   KFLD                    KAMKSC
226000981214     C                   KFLD                    KAMCTR
226100020225     C*
226200020225     C* DATA CORRENTE
226300020225     C                   TIME                    WTIME            14 0          ORA E DATA
226400020225     C                   MOVE      WTIME         DATEU             8 0          GG/MM/AAAA
226500020225     C     *EUR          MOVEL     DATEU         DATA_EUR
226600020225     C     *ISO          MOVEL     DATA_EUR      DATEU                          AAAA/MM/GG
226700120509     c
226800120509     c* carico tabella DPD
226900120509     C                   CLEAR                   DS3IDP
227000120509     C                   MOVEL     1             KTBKUT
227100120509     C                   MOVEL     '3I'          ktbcod
227200120509     C                   MOVEL(P)  'DPD'         ktbkey
227300120509     C     KEYTAB        CHAIN     TABEL00F
227400120509     c                   if        %found(tabel00f) and tblflg=*blanks
227500120509     C                   MOVEL     TBLUNI        DS3IDP
227600120509     c                   endif
227700960617     C*
227800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
227900000000     C                   EXSR      INZSTD
228000981214     C*
228100000000     C* IMPOSTAZIONI INIZIALI VIDEO
228200000000     C                   EXSR      IMPVID
228300000000     C*
228400000000     C* EMETTO FORMATO VIDEO
228500000000     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
228600000000     C*
228700000000     C                   ENDSR
228800000000     C*------------------------------------------------------------------------*
228900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
229000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
229100000000     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA'  A PARTICOLARI STANDARDS
229200000000     C* DI  PROGRAMMAZIONE  E  NELLA  MAGGIOR PARTE DEI  CASI NON E'  NECESSARIO
229300000000     C* MODIFICARLA, NEL CASO FARE ATTENZIONE.
229400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
229500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
229600000000     C*------------------------------------------------------------------------*
229700000000     C* INZSTD - ROUTINE INIZIALE STANDARD
229800000000     C*------------------------------------------------------------------------*
229900000000     C     INZSTD        BEGSR
230000000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
230100000000     C                   CALL      'XKEYS'
230200000000     C                   PARM                    DSKEYS
230300000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
230400000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
230500000000     C     XMSGPA        PLIST
230600000000     C                   PARM                    DSMSMS
230700000000     C                   PARM                    DSMSPG
230800000000     C                   PARM                    DSMSKE
230900000000     C                   PARM                    DSMSER
231000000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
231100970707     C                   MOVEL     'TISI92R'     DSMSPG                         *NOME PGM CHIAMANTE
231200970707     C                   MOVEL     'TISI92R'     VMCQUE                         *CODA MESSAGGI DESTI
231300000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
231400000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
231500000000     C* INIZIALIZZO FLAG CONTROLLO PGM
231600000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
231700000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
231800000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
231900000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
232000000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
232100000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
232200000000     C                   MOVEL     'G'           $TGES             1            *I=INTER. G=GESTIONE
232300000000     C* LANCIO ROUTINE PER VERIFICARE SE
232400000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
232500000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
232600020919     C                   EXSR      REPDATIUTE
232700000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
232800000000     C                   BITOFF    '01234567'    POSMI1            1
232900000000     C                   BITOFF    '01234567'    POSMI2            1
233000000000     C                   BITOFF    '01234567'    POSMA1            1
233100000000     C                   BITOFF    '01234567'    POSMA2            1
233200000000     C                   BITON     '34'          POSMI1            1            *18
233300000000     C                   BITON     '7'           POSMI2            1            *01
233400000000     C                   BITON     '34'          POSMA1            1            *18
233500000000     C                   BITON     '0'           POSMA2            1            *80
233600000000     C                   MOVE      POSMI2        POSMIN            2
233700000000     C                   MOVEL     POSMI1        POSMIN
233800000000     C                   MOVE      POSMA2        POSMAX            2
233900000000     C                   MOVEL     POSMA1        POSMAX
234000000000     C                   ENDSR
234100000000     C*------------------------------------------------------------------------*
234200000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
234300000000     C*------------------------------------------------------------------------*
234400000000     C     FUNSTD        BEGSR
234500000000     C                   SELECT
234600000000     C     KEY           WHENEQ    F3
234700000000     C                   EXSR      F03STD
234800000000     C     KEY           WHENEQ    F12
234900000000     C                   EXSR      F12STD
235000000000     C     KEY           WHENEQ    F24
235100000000     C                   EXSR      F24STD
235200000000     C                   OTHER
235300000000     C                   MOVEL     MSG(2)        $MSG
235400000000     C                   EXSR      CARMSG
235500000000     C                   ENDSL
235600000000     C                   ENDSR
235700000000     C*------------------------------------------------------------------------*
235800000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
235900000000     C*------------------------------------------------------------------------*
236000000000     C     F03STD        BEGSR
236100000000     C*
236200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
236300000000     C                   MOVEL     '1'           $F03
236400000000     C*
236500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
236600000000     C                   MOVEL     '1'           $FINE
236700000000     C*
236800000000     C                   ENDSR
236900000000     C*------------------------------------------------------------------------*
237000000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
237100000000     C*------------------------------------------------------------------------*
237200000000     C     F12STD        BEGSR
237300000000     C*
237400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
237500000000     C                   MOVEL     '1'           $FINE
237600000000     C*
237700000000     C                   ENDSR
237800000000     C*------------------------------------------------------------------------*
237900000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
238000000000     C*------------------------------------------------------------------------*
238100000000     C     F24STD        BEGSR
238200000000     C*
238300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
238400000000     C                   EXSR      OUTFUN
238500000000     C*
238600000000     C                   ENDSR
238700000000     C*------------------------------------------------------------------------*
238800000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
238900000000     C*          CODA MESSAGGI DEL PGM
239000000000     C*------------------------------------------------------------------------*
239100000000     C     CARMSG        BEGSR
239200000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
239300000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
239400000000     C     MSGULT        IFEQ      0
239500000000     C                   MOVEL     '1'           *IN40
239600970707     C                   WRITE     SI92DVK
239700000000     C                   MOVEL     '0'           *IN40
239800000000     C                   ENDIF
239900000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
240000000000     C                   MOVEL     $MSG          DSMSMS
240100000000     C                   CALL      'XMSG'        XMSGPA
240200000000     C     DSMSER        IFEQ      0
240300000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
240400000000     C                   ENDIF
240500000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
240600000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
240700970707     C                   WRITE     SI92DVM                                      *SFL MESS
240800000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
240900000000     C                   MOVEL     *BLANKS       $MSG
241000000000     C                   ENDSR
241100000000     C*------------------------------------------------------------------------*
241200000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
241300000000     C*------------------------------------------------------------------------*
241400000000     C     OUTMSG        BEGSR
241500000000     C* VISUALIZZAZIONE MESSAGGIO
241600000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
241700970707     C                   WRITE     SI92DVK                                      *CONTR MESS
241800000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
241900000000     C                   Z-ADD     *ZEROS        MSGULT
242000000000     C                   ENDSR
242100000000     C*------------------------------------------------------------------------*
242200000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
242300000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
242400000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
242500000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
242600000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
242700000000     C*------------------------------------------------------------------------*
242800000000     C     CARFUN        BEGSR
242900000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
243000000000     C     1             DO        31            I                 5 0          --- 1 -->
243100000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
243200000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
243300000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
243400020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
243500000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
243600000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
243700000000     C                   ELSE                                                   *INTERROG.
243800000000     C                   MOVEL     FUNASI        FAT(I)
243900000000     C                   ENDIF
244000000000     C                   ELSE                                                   <-- 2 --> *FILIALE
244100000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
244200000000     C                   MOVEL     FUNAFG        FAT(I)
244300000000     C                   ELSE                                                   *INTERROG.
244400000000     C                   MOVEL     FUNAFI        FAT(I)
244500000000     C                   ENDIF
244600000000     C                   ENDIF                                                  <-- 2 ---
244700000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
244800000000     C                   MOVEL     USER          WEDP              3            *PROFILO
244900000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
245000000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
245100000000     C                   ELSE
245200000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
245300000000     C                   ENDIF
245400000000     C                   ENDIF
245500000000     C                   ENDDO                                                  <-- 1 ---
245600000000     C                   ENDSR
245700000000     C*------------------------------------------------------------------------*
245800000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
245900000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
246000000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
246100000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
246200000000     C*------------------------------------------------------------------------*
246300000000     C     OUTFUN        BEGSR
246400000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
246500000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
246600000000     C                   ENDDO
246700000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
246800000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
246900000000     C     1             DO        31            I
247000000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
247100000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
247200000000     C                   ADD       1             FUNNAT
247300000000     C                   ENDIF
247400000000     C                   ENDDO
247500000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
247600000000     C     FUNNAT        IFGT      8
247700000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
247800000000     C                   ENDIF
247900000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
248000000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
248100000000     C                   Z-ADD     8             FUNVIS            5 0
248200000000     C     FAT(23)       IFEQ      'S'
248300000000     C                   SUB       1             FUNVIS
248400000000     C                   ENDIF
248500000000     C     FAT(24)       IFEQ      'S'
248600000000     C                   SUB       1             FUNVIS
248700000000     C                   ENDIF
248800000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
248900000000     C                   Z-ADD     *ZEROS        W                 5 0
249000000000     C                   ADD       1             FUNULT
249100000000     C     FUNULT        IFGT      31
249200000000     C                   Z-ADD     1             FUNULT
249300000000     C                   ENDIF
249400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
249500000000     C     FUNULT        DO        31            I                              --- 1 -->
249600000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
249700000000     C     W             IFLT      FUNVIS                                       --- 2 -->
249800000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
249900000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
250000000000     C     FDE(I)        ANDNE     *BLANKS
250100000000     C     I             IFEQ      23                                           *TOLGO F23
250200000000     C     I             OREQ      24                                           *E F24
250300000000     C                   ELSE
250400000000     C                   ADD       1             W
250500000000     C                   MOVEL     FDE(I)        FUV(W)
250600000000     C                   ENDIF
250700000000     C                   ENDIF                                                  <-- 3 ---
250800000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
250900000000     C                   Z-ADD     I             FUNULT
251000000000     C                   ENDIF                                                  <-- 2 ---
251100000000     C                   ENDDO                                                  <-- 1 ---
251200000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
251300000000     C                   MOVEL     FUV(1)        VFD001
251400000000     C                   MOVEL     FUV(2)        VFD002
251500000000     C                   MOVEL     FUV(3)        VFD003
251600000000     C                   MOVEL     FUV(4)        VFD004
251700000000     C                   MOVEL     FUV(5)        VFD005
251800000000     C                   MOVEL     FUV(6)        VFD006
251900000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
252000000000     C     FAT(24)       ANDEQ     'N'
252100000000     C                   MOVEL     FUV(7)        VFD007
252200000000     C                   MOVEL     FUV(8)        VFD008
252300000000     C                   ELSE                                                   <-- 1 -->
252400000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
252500000000     C     FAT(24)       ANDEQ     'S'
252600000000     C                   MOVEL     FDE(23)       VFD007
252700000000     C                   MOVEL     FDE(24)       VFD008
252800000000     C                   ELSE                                                   <-- 2 -->
252900000000     C                   MOVEL     FUV(7)        VFD007
253000000000     C     FAT(23)       IFEQ      'S'
253100000000     C                   MOVEL     FDE(23)       VFD008
253200000000     C                   ELSE
253300000000     C                   MOVEL     FDE(24)       VFD008
253400000000     C                   ENDIF
253500000000     C                   ENDIF                                                  <-- 2 ---
253600000000     C                   ENDIF                                                  <-- 1 ---
253700000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
253800000000     C                   ENDSR
253900000000     C*------------------------------------------------------------------------*
254000000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
254100000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
254200000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
254300000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
254400000000     C*------------------------------------------------------------------------*
254500000000     C     CONFUN        BEGSR
254600000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
254700000000     C     1             DO        31            I                              --- 1 -->
254800000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
254900000000     C* CONTROLLO SE IL TASTO E' ATTIVO
255000000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
255100000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
255200000000     C                   EXSR      CARMSG
255300000000     C                   MOVEL     '1'           $ERR
255400000000     C                   ENDIF                                                  <-- 3 ---
255500000000     C                   ENDIF                                                  <-- 2 ---
255600000000     C                   ENDDO                                                  <-- 1 ---
255700000000     C                   ENDSR
255800000000     C*------------------------------------------------------------------------*
255900000000** -MSG- MESSAGGI
256000970707TISI92R- Tasto funzionale non valido.                                          1
256100970707TISI92R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
256200970707TISI92R- Opzione non valida.                                                   3
256300970707TISI92R- Opzione abilitata ma non gestita. Avvertire CED.                      4
256400970707TISI92R- Inizio elenco.                                                        5
256500970707TISI92R- Fine elenco.                                                          6
256600970707TISI92R- Data errata                                                           7
256700970707TISI92R- Punto operativo errato                                                8
256800970707TISI92R- Tipo servizio errato                                                  9
256900970707TISI92R- Nazione errata                                                       10
257000090615TISI92R- CAP errato per la nazione indicata                                   11
257100110627TISI92R- Tipo inoltro forzato errato o non utilizzabile                       12
257200971126TISI92R- Tipo porto errato                                                    13
257300970714TISI92R- Località inesistente in questo cap: immettere '?' per la scelta      14
257400980116TISI92R- Immettere uno dei valori possibili                                   15
257500020225TISI92R- Impossibile specificare network per nazione Italia.                  16
257600970707TISI92R- Immettere il numero dei colli                                        17
257700970707TISI92R- Immettere il peso   (in Kg)                                          18
257800970707TISI92R- Immettere il volume (in Mc)                                          19
257900970707TISI92R- Codice cliente errato o non in gestione                              20
258000970707TISI92R- Codice tariffa errato                                                21
258100010823TISI92R- Divisa errata                                                        22
258200970707TISI92R- Tipo incasso C/Assegno errato                                        23
258300970707TISI92R- C/Assegno obbligatorio se immesso la valuta o il tipo incasso        24
258400970707TISI92R- Non sono ammessi decimali per la valuta indicata                     25
258500010820TISI92R- Tipo incasso non utilizzabile per la divisa indicata                 26
258600970707TISI92R- Importo assicurazione obbligatorio se immessa la valuta              27
258700980703TISI92R- Lasciare vuoti i dati del mittente se porto franco                   28
258800980703TISI92R- Immettere i dati del mittente se porto assegnato                     29
258900010823TISI92R- Divisa non più accettabile                                           30
259000020226TISI92R- Network indicato inaccettabile per estero                            31
259100020514TISI92R- Nazione e Network incompatibili.                                     32
259200030320ATTENZIONE!! Volume troppo alto. Ricalcolare.                                 33
259300050413Consegna particolare errata                                                   34
259400050413Non è possibile impostare la stessa consegna particolare 2 volte              35
259500050428Se FERMO DEPOSITO, Inoltro non forzabile                                      36
259600090429E' stata immessa una tariffa BLOCCATA. Enter forza                            37
259700090429Tariffa valida solo in Partenza non corretta per porti Assegnati.enter forza  38
259800090429Tariffa valida solo in Arrivo  non corretta per porti franco.enter forza      39
259900090615Tariffa incongruente col Network selezionato. Enter forza                     40
260000090617Network non utilizzabile per tutte le spedizioni:ENTER forza                  41
260100090617Network non utilizzabile per spedizioni con C/Assegno:ENTER forza             42
260200090617Network non utilizzabile per spedizioni in P.Assegnato:ENTER forza            43
260300090617Network non utilizzabile per spedizioni in P.Ass.o C/ASS:ENTER forza          44
260400090617Assegnato export per il network solo con ORM reale commissionato.ENTR forza   45
260500110923Superati i limiti per tipo servizio. Enter Forza                              46
260600120509Spedizione D.P.D. : deve essere di 1 collo                                    47
260700120509Spedizione D.P.D. : non ammesso un peso superiore a                           48
260800170120Importo da assicurare non accettato: non rientra nei limiti previsti          49
260900000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
261000000000F1    -                   -    NN     NN
261100000000F2    -                   -    NN     NN
261200000000F3    -F3=Fine            -    SS     SS
261300000000F4    -                   -    NN     NN
261400000000F5    -                   -    NN     NN
261500000000F6    -F6=Lancio          -    SN     SN
261600000000F7    -                   -    NN     NN
261700000000F8    -                   -    NN     NN
261800000000F9    -                   -    NN     NN
261900000000F10   -                   -    NN     NN
262000030320F11   -F11=Volume         -    SN     SN
262100000000F12   -F12=Ritorno        -    SS     SS
262200000000F13   -                   -    NN     NN
262300000000F14   -                   -    NN     NN
262400000000F15   -                   -    NN     NN
262500000000F16   -                   -    NN     NN
262600000000F17   -                   -    NN     NN
262700000000F18   -                   -    NN     NN
262800000000F19   -                   -    NN     NN
262900000000F20   -                   -    NN     NN
263000000000F21   -                   -    NN     NN
263100000000F22   -                   -    NN     NN
263200000000F23   -                   -    NN     NN
263300000000F24   -F24=Altri tasti    -    NN     NN
263400000000ENTER -                   -    SS     SS
263500000000ROLLUP-                   -    NN     NN
263600000000ROLLDN-                   -    NN     NN
263700000000HELP  -                   -    NN     NN
263800000000PRINT -                   -    NN     NN
263900000000BSPACE-                   -    NN     NN
264000000000CLEAR -                   -    NN     NN
