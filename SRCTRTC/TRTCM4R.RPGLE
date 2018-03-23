000100970526     H*------------------------------------------------------------------------*
000200030822     H* Gestione file di log INVIO SPOOL VIA E-MAIL (TIMAL00F)
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030509     FTRTCM4D   CF   E             WORKSTN INFDS(DEVDS1)
000900030509     F                                     SFILE(TCM4DMS:MSGULT)
001000030509     Ftimal01l  UF   E           K DISK
001100031210     Ftimar01l  IF   E           K DISK
001200970526     D*------------------------------------------------------------------------*
001300970526     D* SCHIERE
001400970526     D*------------------------------------------------------------------------*
001500970526     D*------------
001600970526     D* DECODIFICHE
001700970526     D*------------
001800970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
001900970526     D*------------
002000970526     D* MESSAGGI
002100970526     D*------------
002200970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* DESCRIZIONI TASTI FUNZIONALI
002500970526     D*------------
002600970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* FUNZIONI DA CARICARE A VIDEO
002900970526     D*------------
003000970526     D FUV             S             19    DIM(24)
003100970526     D*------------------------------------------------------------------------*
003200970526     D* INPUT
003300970526     D*------------------------------------------------------------------------*
003400970526     D*------------
003500970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003600970526     D*------------
003700970526     D FUDDS           DS
003800970526     D  FUDDE1                 1      1
003900970526     D  FUDFUN                 2      3
004000970526     D  FUDDE2                 4     19
004100970526     D  FUDFI0                20     34
004200970526     D  FUDATT                35     35
004300970526     D  FUDFI1                36     80
004400970526     D  FUDDES                 1     19
004500970526     D  FUDRIG                 1     80
004600970526     D*------------
004700970526     D* DS GESTIONE TASTI FUNZIONALI
004800970526     D*------------
004900970526     D DSKEY         E DS
005000970526     D DEVDS1          DS
005100970526     D  KEY                  369    369
005200970526     D*------------
005300970526     D* DS "XMSG" - GESTIONE MESSAGGI
005400970526     D*------------
005500970526     D DSMSG         E DS
005600021219     D*------------
005700970526     D* ARCHITETTURA
005800970526     D*------------
005900970526     D KPJBA         E DS
006000030509     D*------------
006100030509     D* DS DI PROCEDURA
006200030509     D*------------
006300030509     D TRTCM1DS      E DS
006400021219     D*------------------
006500021219     D* PASSAGGIO PARAMETRI
006600021219     D*------------------
006700050111     D PARAM           DS                  INZ
006800050111     D  POP1                   1      3
006900050111     D  POP2                   4      4
007000050111     D  PPRG                   5     11
007100050111     D  PPDA                  12     13
007200050111     D  PPA                   14     15
007300050111     D  PMSG                  16     95
007400030822     D*------------------
007500030822     D* PASSAGGIO PARAMETRI X GESTIONE REGOLE
007600030822     D*------------------
007700030822     D PARAMREG        DS                  INZ
007800030822     D  POP0R                  1      3
007900030822     D  POP1R                  4      6
008000030822     D  PCODR                  7      9
008100030822     D  PMSGR                 10     89
008200021219     D*-------------------
008300021219     D* DS REPERIMENTO DATI UTENTE
008400021219     D*-------------------
008500021219     D TIBS34DS      E DS                                                       *Profili utente
008600021219     D DDATIUTE      E DS                                                       *Dati utente
008700021219     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008800970526     C*------------------------------------------------------------------------*
008900970526     C* MAIN LINES
009000970526     C*------------------------------------------------------------------------*
009100970526     C*
009200970526     C* NON CI SONO ERRORI IN ENTRATA
009300021211     C     ERRL00        IFEQ      '0'
009400970526     C*
009500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009600021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009900970526     C*
010000970526     C* EMETTE FORMATO VIDEO
010100970526     C                   EXSR      BUTFMR
010200970526     C*
010300970526     C* GESTIONE TASTI FUNZIONALI
010400970526     C                   EXSR      FUNCON
010500021211     C                   ENDDO
010600970526     C*
010700021211     C                   ENDIF
010800021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
010900021211     C                   MOVEL     PARAM         KPJBU
011000970526     C* FINE PGM
011100970526     C                   SETON                                        LR
011200970526     C*------------------------------------------------------------------------*
011300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011400970526     C*------------------------------------------------------------------------*
011500970526     C     BUTFMR        BEGSR
011600970526     C*
011700970526     C* EMETTE LA TESTATA
011800030509     C                   WRITE     TCM4D01
011900970526     C*
012000970526     C* SPEDISCE EVENTUALE MESSAGGIO
012100970526     C                   EXSR      SNDMSG
012200970526     C*
012300970526     C* EMETTE LE FUNZIONI
012400030509     C                   WRITE     TCM4D02
012500970526     C*
012600970526     C* EMETTE IL VIDEO
012700030509     C                   EXFMT     TCM4D03
012800970526     C*
012900970526     C* REIMPOSTA FLAG DI ERRORE
013000970526     C                   MOVEL     '0'           ERRL00
013100970526     C*
013200970526     C                   ENDSR
013300970526     C*------------------------------------------------------------------------*
013400970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013500970526     C*------------------------------------------------------------------------*
013600970526     C     SNDMSG        BEGSR
013700970526     C*
013800970526     C* PULISCE IL SFILE MESSAGGI
013900970526     C                   MOVEL     '1'           *IN40
014000030509     C                   WRITE     TCM4DMC
014100970526     C                   MOVEL     '0'           *IN40
014200970526     C*
014300970526     C* VISUALIZZA IL MESSAGGIO
014400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014500970526     C                   CALL      'XMSG'        XMSGPA
014600021211     C     DSMSER        IFEQ      *ZEROS
014700970526     C                   MOVEL     DSMSKE        MSGKEY
014800021211     C                   ENDIF
014900970526     C                   Z-ADD     1             MSGULT
015000030509     C                   WRITE     TCM4DMS
015100030509     C                   WRITE     TCM4DMC
015200970526     C*
015300970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015400970526     C                   MOVEL     *BLANKS       DSMSMS
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800021219     C* ENTGES - GESTIONE ENTER -> INVIO
015900970526     C*------------------------------------------------------------------------*
016000970526     C     ENTGES        BEGSR
016100970526     C*
016200970526     C* CONTROLLA I CAMPI IMMESSI
016300030822     C                   EXSR      DECODI
016400970526     C*
016500970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
016600021211     C     POP1          IFEQ      'O05'
016700021211     C                   MOVEL     *BLANKS       PMSG
016800031210     C                   EVAL      DSMSMS = *BLANKS
016900970526     C                   MOVEL     '1'           FINL00
017000021211     C                   ENDIF
017100970526     C*
017200970526     C                   ENDSR
017300040818     C*------------------------------------------------------------------------*
017400040818     C* F01GES - GESTIONE F01 -> HELP CAMPO CORRENTE
017500040818     C*------------------------------------------------------------------------*
017600040818     C     F01GES        BEGSR
017700040818     C*
017800040818     C* IMPOSTO A VIDEO I DEFAULT
017900040818     C                   MOVEL     *blanks       wVWDDES        1000
018000040818     C                   EVAL      wVWDDES = 'HELP NON CONTEMPLATO'
018100040818     C*
018200040818     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE X EMETTERE L'HELP CORRELATO
018300041005     C                   IF        RNMFL = 'VIDVAR'
018400040818     C* IMPOSTO A VIDEO IL NOME DEL CAMPO CORRENTE
018500040818     C                   EVAL      VWDFLD = '"Parametri variabili"'
018600040818     C* IMPOSTO A VIDEO LA DESCRIZIONE DEL CAMPO
018700040818     C                   EVAL      wVWDDES = 'Indica gli eventuali parametri ' +
018800040819     C                             'ad uso variabile; può contenere valori '  +
018900040818     C                             'costanti e/o tag riconosciuti. Attualmente'+
019000040818     C                             ' i tag riconosciuti sono: *PATHSF* + valor'+
019100040818     C                             'e => indica la cartella dove salvare; *OBJ'+
019200040818     C                             'M* + valore => indica oggetto della e-ma'+
019300040818     C                             'il. Tale campo è comunque subordinato alla'+
019400040818     C                             ' regola presente sul record di log.'
019500040818     C                   ENDIF
019600040818     C*
019700040818     C                   EVAL      VWDDES1 = %subst(wVWDDES:1:50)
019800040818     C                   EVAL      VWDDES2 = %subst(wVWDDES:51:50)
019900040818     C                   EVAL      VWDDES3 = %subst(wVWDDES:101:50)
020000040818     C                   EVAL      VWDDES4 = %subst(wVWDDES:151:50)
020100040818     C                   EVAL      VWDDES5 = %subst(wVWDDES:201:50)
020200040818     C                   EVAL      VWDDES6 = %subst(wVWDDES:251:50)
020300040818     C                   EVAL      VWDDES7 = %subst(wVWDDES:301:50)
020400040818     C                   EVAL      VWDDES8 = %subst(wVWDDES:351:50)
020500040818     C*
020600040818     C* QUINDI EMETTO LA HELP-WINDOWS
020700040818     C                   EXFMT     TCM4DWW
020800040818     C*
020900040818     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* F03GES - GESTIONE F03 -> FINE
021200970526     C*------------------------------------------------------------------------*
021300970526     C     F03GES        BEGSR
021400970526     C*
021500970526     C* IMPOSTA "PREMUTO F03"
021600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021700970526     C*
021800970526     C                   ENDSR
021900030509     C*------------------------------------------------------------------------*
022000030509     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
022100030509     C*------------------------------------------------------------------------*
022200030509     C     F06GES        BEGSR
022300031210     C*
022400031210     C* ESEGUO ROUTINE DEI CONTROLLI
022500031210     C                   EXSR      DECODI
022600041122     C*
022700041122     C* SE NO ERRORI RILEGGO IL RECORD ALLOCANDOLO
022800041122     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022900041122     C     KEYMAL        CHAIN     TIMAL01L
023000041122     C                   ENDIF
023100030509     C*
023200030509     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
023300030509     C     POP1          IFEQ      'O02'
023400030509     C                   EXSR      MODIFI
023500030509     C                   ENDIF
023600031210     C*
023700031210     C     POP1          IFEQ      'O04'
023800031210     C                   EXSR      ELIMINA
023900031210     C                   ENDIF
024000031210     C*
024100031210     C     POP1          IFEQ      'O06'
024200031210     C                   EXSR      RILASCIO
024300031210     C                   ENDIF
024400030509     C*
024500030509     C                   ENDSR
024600030822     C*------------------------------------------------------------------------*
024700030822     C* F09GES - GESTIONE F09 -> VISUALIZZA REGOLA
024800030822     C*------------------------------------------------------------------------*
024900030822     C     F09GES        BEGSR
025000030822     C*
025100030822     C* VERIFICO CHE IL CODICE REGOLA SIA VALORIZZATO
025200041005     C                   IF        VIDTIPS <> *blanks
025300030822     C*
025400030822     C* SALVO LA KPJBU ORIGINALE
025500030822     C                   EVAL      SAVKPJBU = KPJBU
025600030822     C*
025700030822     C* RICHIAMA IL PROGRAMMA GESTIONE REGOLE IN MODALITA' "VISUALIZZAZIONE"
025800030822     C                   CLEAR                   PARAMREG
025900030822     C                   EVAL      POP1R = 'O05'
026000041005     C                   EVAL      PCODR = VIDTIPS
026100030822     C                   EVAL      KPJBU = PARAMREG
026200030822     C                   CALL      'TRTCM6R'
026300030822     C                   PARM                    KPJBA
026400030822     C*
026500030822     C* RIPRISTINO LA KPJBU ORIGINALE
026600030822     C                   EVAL      KPJBU = SAVKPJBU
026700030822     C                   ELSE
026800030822     C                   MOVEL     MSG(4)        DSMSMS
026900030822     C                   ENDIF
027000030822     C*
027100030822     C                   ENDSR
027200970526     C*------------------------------------------------------------------------*
027300970526     C* F12GES - GESTIONE F12 -> RITORNO
027400970526     C*------------------------------------------------------------------------*
027500970526     C     F12GES        BEGSR
027600970526     C*
027700970526     C* IMPOSTA "PREMUTO F12"
027800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
027900970526     C*
028000970526     C                   ENDSR
028100970526     C*------------------------------------------------------------------------*
028200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
028300970526     C*------------------------------------------------------------------------*
028400970526     C     F24GES        BEGSR
028500970526     C*
028600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
028700970526     C*
028800970526     C                   ENDSR
028900030822     C*------------------------------------------------------------------------*
029000031210     C* DECODI - GESTIONE CONTROLLO E DECODIFICHE CAMPI A VIDEO
029100030822     C*------------------------------------------------------------------------*
029200030822     C     DECODI        BEGSR
029300031211     C*
029400031211     C* EFFETTUO I CONTROLLI SOLO X LE OPZIONI CHE IMPLICANO UNA ITERAZIONE CON L'UTENTE
029500031211     C                   IF        POP1 = 'O02' OR
029600031211     C                             POP1 = 'F06'
029700040702     C*
029800040702     C* VERIFICO ED EVENTUALMENTE NORMALIZZO I PARAMETRI VARIABILI
029900041005     C                   IF        VIDVAR <> *blanks
030000041023     C                   IF        %subst(%trim(VIDVAR):1:1) = '§'
030100041005     C                   EVAL      VIDVAR = %subst(%trim(VIDVAR):2)
030200040702     C                   ENDIF
030300041005     C                   IF        %subst(%trim(VIDVAR):%len(%trim(VIDVAR)):1)
030400041023     C                             = '§'
030500041005     C                   EVAL      VIDVAR =
030600041005     C                             %subst(%trim(VIDVAR):1:%len(%trim(VIDVAR))-
030700040702     C                             1)
030800040702     C                   ENDIF
030900041005     C                   EVAL      VIDVAR = %trim(VIDVAR)
031000040702     C                   ENDIF
031100031210     C*
031200031210     C* NORMALIZZO VALORE '*NONE'
031300041005     C                   IF        VIDMITT = '*none'
031400041005     C                   EVAL      VIDMITT = '*NONE'
031500031210     C                   ENDIF
031600031210     C*
031700031210     C* VERIFICO CHE ALMENO O IL CODICE REGOLA O I DATI UTENTE ESTESI SIANO VALORIZZATI
031800041005     C                   IF        VIDTIPS  = *blanks
031900031210     C                   MOVEL     '1'           ERRL00                         *ERRORE
032000031210     C                   MOVEL     MSG(10)       DSMSMS
032100031210     C                   IF        VIDSTS = '0'
032200031210     C                   EVAL      VIDSTS = 'H'
032300031210     C                   ENDIF
032400031210     C                   ENDIF
032500030822     C*
032600030822     C* VERIFICO SE RICHIESTO ELENCO REGOLE ESISTENTI
032700041005     C                   IF        VIDTIPS <> *blanks
032800041005     C     '?'           SCAN      VIDTIPS       wScan             3 0
032900030822     C                   IF        wScan > *zeros
033000030822     C*
033100030822     C* SALVO LA KPJBU ORIGINALE
033200030822     C                   EVAL      SAVKPJBU = KPJBU
033300030822     C*
033400030822     C* RICHIAMA IL PROGRAMMA GESTIONE REGOLE IN MODALITA' "VISUALIZZAZIONE"
033500030822     C                   CLEAR                   PARAMREG
033600030822     C                   EVAL      POP0R = 'P01'
033700030822     C                   EVAL      POP1R = 'P01'
033800030822     C                   EVAL      KPJBU = PARAMREG
033900030822     C                   CALL      'TRTCM5R'
034000030822     C                   PARM                    KPJBA
034100030822     C                   EVAL      PARAMREG = KPJBU
034200041005     C                   EVAL      VIDTIPS  = PCODR
034300030822     C*
034400030822     C* RIPRISTINO LA KPJBU ORIGINALE
034500030822     C                   EVAL      KPJBU = SAVKPJBU
034600031210     C                   ELSE
034700031210     C     KEYMAR        CHAIN     timar01l
034800031210     C                   IF        %found(timar01l)
034900041005     C                   IF        VIDMITT = '*NONE'
035000041005     C                   EVAL      VIDMITT = *blanks
035100031210     C                   ENDIF
035200031210     C                   ELSE
035300031210     C                   MOVEL     '1'           ERRL00                         *ERRORE
035400031210     C                   MOVEL     MSG(9)        DSMSMS
035500031210     C                   IF        VIDSTS = '0'
035600031210     C                   EVAL      VIDSTS = 'H'
035700031210     C                   ENDIF
035800031210     C                   ENDIF
035900031210     C                   ENDIF
036000031210     C                   ENDIF
036100031211     C                   ENDIF
036200031210     C*
036300030822     C                   ENDSR
036400031210     C*------------------------------------------------------------------------*
036500031210     C* ELIMINA - OPERAZIONI DI CANCELLAZIONE RECORD
036600031210     C*------------------------------------------------------------------------*
036700031210     C     ELIMINA       BEGSR
036800031210     C*
036900031210     C* INCREMENTO CONTATORE PRESSIONI "F06"
037000031210     C                   ADD       1             wF06
037100031210     C*
037200031210     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
037300031210     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
037400031210     C* PER PRIMA COSA ELIMINO IL FILE RELATIVO NELL'IFS
037500031211     C                   EVAL      OPZIONE = 'O04'
037600031210     C                   CALL      'TRTCM4C'
037700031211     C                   PARM                    OPZIONE           3
037800031210     C                   PARM                    MALFNAM
037900031210     C                   PARM                    MALJNAM
038000031210     C                   PARM                    MALUNAM
038100031210     C                   PARM                    MALJNUM
038200031210     C                   PARM                    MALFNUM
038300031210     C                   PARM                    ESITO             1
038400031210     C* SE ELIMINAZIONE FILE NEL'IFS TUTTO OK PROCEDO CON L'ELIMINAZIONE DEL RECORD
038500031210     C                   IF        ESITO <> '2' OR wF06 > 1
038600031210     C                   DELETE    TIMAL000
038700031210     C                   MOVEL     '1'           F03L00                         *USCITA PGM
038800031210     C                   EVAL      PMSG = MSG(6)
038900031210     C                   EVAL      DSMSMS = MSG(6)
039000031210     C                   ELSE
039100031210     C                   EVAL      PMSG = MSG(7)
039200031210     C                   EVAL      DSMSMS = MSG(7)
039300031210     C                   ENDIF
039400031210     C                   ENDIF
039500031210     C*
039600031210     C                   ENDSR
039700030509     C*------------------------------------------------------------------------*
039800030509     C* MODIFI - OPERAZIONI DI MODIFICA RECORD
039900030509     C*------------------------------------------------------------------------*
040000030509     C     MODIFI        BEGSR
040100030509     C*
040200030509     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
040300030509     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040400041005     C                   EVAL      MALIDP    = VIDIDP
040500041005     C                   EVAL      MALATT    = VIDATT
040600041005     C                   EVAL      MALTOTATT = VIDTOTATT
040700041005     C                   EVAL      MALPROG   = VIDPROG
040800041005     C                   EVAL      MALFNAM   = VIDFNAM
040900041005     C                   EVAL      MALFNUM   = VIDFNUM
041000080526     C                   EVAL      MALSYSN   = VIDSYSN
041100041005     C                   EVAL      MALTPAG   = VIDTPAG
041200041005     C                   EVAL      MALUNAM   = VIDUNAM
041300041005     C                   EVAL      MALJNAM   = VIDJNAM
041400041005     C                   EVAL      MALJNUM   = VIDJNUM
041500050418     C                   IF        MALSTS <> 'X'
041600041005     C                   EVAL      MALSTS    = VIDSTS
041700050418     C                   ENDIF
041800050111     C                   EVAL      MALMSG    = VIDMSG
041900041005     C                   EVAL      MALUDTA   = VIDUDTA
042000041005     C                   EVAL      MALPRTFL  = VIDPRTFL
042100041005     C                   EVAL      MALSPCPI  = VIDSPCPI * 10
042200060706     C                   EVAL      MALPRTTY  = VIDPRTTY
042300041005     C                   EVAL      §CM1VAR   = VIDVAR
042400041005     C                   EVAL      §CM1MITT  = VIDMITT
042500050111     C                   EVAL      §CM1DST   = VIDDST
042600041005     C                   EVAL      §CM1TIPS  = VIDTIPS
042700041005     C                   EVAL      §CM1PO    = VIDPO
042800060731     C                   EVAL      §CM1PCL   = VIDPCL
042900030509     C                   MOVEL(P)  TRTCM1DS      MALDUEX
043000030509     C                   UPDATE    TIMAL000
043100030509     C                   MOVEL     '1'           F03L00                         *USCITA PGM
043200030509     C                   EVAL      PMSG = MSG(2)
043300031210     C                   EVAL      DSMSMS = MSG(2)
043400030509     C                   ENDIF
043500030509     C*
043600030509     C                   ENDSR
043700031210     C*------------------------------------------------------------------------*
043800031210     C* RILASCIO - OPERAZIONI DI RILASCIO RECORD
043900031210     C*------------------------------------------------------------------------*
044000031210     C     RILASCIO      BEGSR
044100031210     C*
044200031210     C* VERIFICO SE RECORD DI LOG E' CONGELATO
044300050418     C                   IF        MALSTS = '0' OR MALSTS = 'X'
044400040924     C                   EVAL      PMSG = MSG(11)
044500040924     C                   EVAL      DSMSMS = MSG(11)
044600031210     C                   ELSE
044700031210     C*
044800031210     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
044900031210     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
045000031210     C                   EVAL      MALSTS   = '0'
045100031210     C                   UPDATE    TIMAL000
045200031210     C                   MOVEL     '1'           F03L00                         *USCITA PGM
045300031210     C                   EVAL      PMSG = MSG(5)
045400031210     C                   EVAL      DSMSMS = MSG(5)
045500031210     C                   ENDIF
045600031210     C                   ENDIF
045700031210     C*
045800031210     C                   ENDSR
045900021219     C*------------------------------------------------------------------------*
046000030509     C* LETREC - LEGGE IL RECORD E LO CARICA A VIDEO
046100021219     C*------------------------------------------------------------------------*
046200030509     C     LETREC        BEGSR
046300021219     C*
046400041122     C     KEYMAL        CHAIN(N)  TIMAL01L                           99
046500030509     C                   IF        %found(TIMAL01L)
046600041005     C                   EVAL      VIDIDP    = MALIDP
046700041005     C                   EVAL      VIDATT    = MALATT
046800041005     C                   EVAL      VIDTOTATT = MALTOTATT
046900041005     C                   EVAL      VIDPROG   = MALPROG
047000050112     C                   EVAL      VIDDAOR = %subst(MALDAOR:9:2) + '/' +
047100050112     C                                       %subst(MALDAOR:6:2) + '/' +
047200050112     C                                       %subst(MALDAOR:1:4) + ' ' +
047300050112     C                                       %subst(MALDAOR:12:8)
047400041005     C                   EVAL      VIDFNAM   = MALFNAM
047500041005     C                   EVAL      VIDFNUM   = MALFNUM
047600080526     C                   EVAL      VIDSYSN   = MALSYSN
047700041005     C                   EVAL      VIDTPAG   = MALTPAG
047800041005     C                   EVAL      VIDPRTFL  = MALPRTFL
047900041005     C                   EVAL      VIDSPCPI  = MALSPCPI / 10
048000060731     C                   EVAL      VIDPRTTY  = MALPRTTY
048100041005     C                   EVAL      VIDUNAM   = MALUNAM
048200041005     C                   EVAL      VIDJNAM   = MALJNAM
048300041005     C                   EVAL      VIDJNUM   = MALJNUM
048400050418     C                   IF        MALSTS = 'X'
048500050418     C                   EVAL      VIDSTS    = '0'
048600050418     C                   ELSE
048700041005     C                   EVAL      VIDSTS    = MALSTS
048800050418     C                   ENDIF
048900050111     C                   EVAL      VIDMSG    = MALMSG
049000041005     C                   EVAL      VIDUDTA   = MALUDTA
049100030509     C                   MOVEL(P)  MALDUEX       TRTCM1DS
049200041005     C                   EVAL      VIDTIPS   = §CM1TIPS
049300050111     C*
049400050111     C* gestisco che se richiamo da filiale i parametri variabili assumono il solo
049500050111     C* significato d "Oggetto mail"
049600050111     C                   IF        *IN50 = *ON
049700050111     C                   EVAL      VIDVAR = %subst(§CM1VAR:%scan('*OBJM*':
049800050111     C                                      §CM1VAR)+%len('*OBJM*'))
049900050111     C                   ELSE
050000050111     C                   EVAL      VIDVAR    = §CM1VAR
050100050111     C                   ENDIF
050200050111     C*
050300041005     C                   EVAL      VIDMITT   = §CM1MITT
050400050111     C                   EVAL      VIDDST    = §CM1DST
050500041005     C                   EVAL      VIDPO     = §CM1PO
050600060721     C                   EVAL      VIDPCL    = §CM1PCL
050700030509     C                   ELSE
050800030509     C                   MOVEL     '1'           ERRL00                         *ERRORE
050900030509     C                   MOVEL     '1'           F03L00                         *USCITA PGM
051000030509     C                   EVAL      PMSG = MSG(3)
051100031210     C                   EVAL      DSMSMS = MSG(3)
051200030509     C                   ENDIF
051300021219     C*
051400021219     C                   ENDSR
051500970526     C*------------------------------------------------------------------------*
051600970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
051700970526     C*------------------------------------------------------------------------*
051800970526     C     IMPFUN        BEGSR
051900050111     C*
052000050111     C* SE CHIAMATA DA FILIALE TOGLIE LA FUNZIONE DI RICHIAMO ANAGRAFICA REGOLE
052100050111     C     POP2          IFEQ      'F'
052200050111     C                   MOVEL     FUD(9)        FUDRIG                         *ANAG. REGOLE
052300050111     C                   MOVEL     'N'           FUDATT
052400050111     C                   MOVEL     FUDRIG        FUD(9)
052500050111     C                   ENDIF
052600970526     C*
052700970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
052800021211     C     POP1          IFEQ      'O05'
052900970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
053000970526     C                   MOVEL     'N'           FUDATT
053100970526     C                   MOVEL     FUDRIG        FUD(6)
053200021211     C                   ENDIF
053300970526     C*
053400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
053500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
053600970526     C                   EXSR      FUNGES
053700970526     C*
053800000407     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
053900021219     C     POP1          IFNE      'F06'                                        *NO INSERIMENTO
054000030509     C                   EXSR      LETREC
054100980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
054200000407     C                   ENDIF
054300970526     C*
054400030509     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
054500021211     C     POP1          IFEQ      'O02'
054600021211     C                   MOVEL     DEC(1)        VIDFUN
054700021211     C                   ENDIF
054800021211     C     POP1          IFEQ      'O05'
054900030509     C                   MOVEL     DEC(2)        VIDFUN
055000021211     C                   ENDIF
055100031210     C     POP1          IFEQ      'O04'
055200031210     C                   MOVEL     DEC(3)        VIDFUN
055300031210     C                   ENDIF
055400031210     C     POP1          IFEQ      'O06'
055500031210     C                   MOVEL     DEC(4)        VIDFUN
055600031210     C                   ENDIF
055700021211     C*
055800021211     C                   ENDSR
055900021219     C*--------------------------------------------------------------------------------------------*
056000021219     C* REPERISCE I DATI UTENTE
056100021219     C*--------------------------------------------------------------------------------------------*
056200021219     C     REPDATIUTE    BEGSR
056300021219     C*
056400021219     C* INIZIALIZZA VARIABILI DI WRK
056500021219     C                   CLEAR                   TIBS34DS
056600021219     C                   CLEAR                   AZUTEDS
056700021219     C                   CLEAR                   DDATIUTE
056800021219     C*
056900021219     C     *DTAARA       DEFINE    §azute        azuteds
057000021219     C     *DTAARA       DEFINE    §datiute      ddatiute
057100021219     C                   IN(E)     *DTAARA
057200021219     C                   IF        %Error
057300021220     C                   EVAL      I34Tla = 'L'
057400021219     C                   CALL      'TIBS34R'
057500021219     C                   PARM                    Tibs34Ds
057600021219     C                   IN        *DTAARA
057700021219     C                   ENDIF
057800021219     C*
057900021219     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
058000021219     C                   MOVEL(P)  RSUT          DSFIRS           20
058100021219     C*
058200021219     C                   ENDSR
058300970526     C*------------------------------------------------------------------------*
058400970526     C* *INZSR - OPERAZIONI INIZIALI
058500970526     C*------------------------------------------------------------------------*
058600970526     C     *INZSR        BEGSR
058700970526     C*---
058800970526     C* RICEVIMENTO PARAMETRI
058900970526     C*---
059000970526     C     *ENTRY        PLIST
059100970612     C                   PARM                    KPJBA
059200000407     C                   MOVEL     KPJBU         PARAM
059300021219     C*
059400021219     C* REPERISCE I DATI DELL'UTENTE
059500021219     C                   EXSR      REPDATIUTE
059600050111     C*
059700050111     C* VERIFICO SE TRATTASI D CHIAMATA DA SEDE O FILIALE
059800050111     C                   SETOFF                                       50
059900050111     C                   IF        POP2 = 'F'
060000050111     C                   SETON                                        50
060100050111     C                   ENDIF
060200000407     C*
060300000407     C* EFFETTUO PRIME VERIFICHE SUL TIPO DI OPERAZIONI DA EFFETTUARE
060400031210     C     POP1          IFEQ      'O04'
060500031210     C                   SETON                                        90
060600031210     C                   ENDIF
060700000407     C     POP1          IFEQ      'O05'
060800021230     C                   SETON                                        90
060900000407     C                   ENDIF
061000031210     C     POP1          IFEQ      'O06'
061100031210     C                   SETON                                        90
061200031210     C                   ENDIF
061300030509     C     POP1          IFEQ      'O02'
061400030509     C                   SETOFF                                       90
061500030509     C                   ENDIF
061600021219     C*---
061700021219     C* CHIAVI DI LETTURA
061800021219     C*---
061900030509     C* LETTURA TIMAL01L - COMPLETA
062000030509     C     KEYMAL        KLIST
062100041006     C                   KFLD                    PPRG                           *PROGR. UNIVOCO ID
062200041006     C                   KFLD                    PPDA                           *PROGR. UNIVOCO DA
062300031210     C* LETTURA TIMAR01L - COMPLETA
062400031210     C     KEYMAR        KLIST
062500041005     C                   KFLD                    VIDTIPS                        *CODICE TIPO REGOLA
062600970526     C*
062700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
062800970526     C     XMSGPA        PLIST
062900970526     C                   PARM                    DSMSMS
063000970526     C                   PARM                    DSMSPG
063100970526     C                   PARM                    DSMSKE
063200970526     C                   PARM                    DSMSER
063300970526     C*
063400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
063500970526     C                   CALL      'XKEY'
063600970526     C                   PARM                    DSKEY
063700970526     C*---
063800970526     C* VARIABILI NON RIFERITE AL DATA BASE
063900970526     C*---
064000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
064100030822     C     *LIKE         DEFINE    KPJBU         SAVKPJBU
064200970526     C*---
064300970526     C* VARIABILI DI CONTROLLO
064400970526     C*---
064500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
064600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
064700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
064800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
064900031210     C                   Z-ADD     *ZEROS        wF06              1 0          *# F06
065000970526     C*
065100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
065200030509     C                   MOVEL     'TRTCM4R'     DSMSPG
065300030509     C                   MOVEL     'TRTCM4R'     MSGQUE
065400970526     C                   MOVEL     *BLANKS       MSGKEY
065500970526     C                   MOVEL     *BLANKS       DSMSMS
065600970526     C*
065700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
065800970526     C                   EXSR      IMPFUN
065900970526     C*
066000970526     C                   ENDSR
066100970526     C*------------------------------------------------------------------------*
066200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
066300970526     C*------------------------------------------------------------------------*
066400970526     C     FUNGES        BEGSR
066500970526     C*
066600970526     C* PULISCO CAMPI DI LAVORO
066700970526     C                   Z-ADD     0             W                 5 0
066800970526     C*
066900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
067000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
067100970526     C     1             DO        24            I
067200970526     C                   MOVEL     FUD(I)        FUDRIG
067300970526     C*
067400970526     C* PULIZIA FUNZIONI PRECEDENTI
067500970526     C                   MOVEL     *BLANKS       FUV(I)
067600970526     C*
067700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
067800970526     C* (SALTA F24)
067900970526     C     FUDATT        IFEQ      'S'
068000970526     C     FUDFUN        ANDNE     '24'
068100970526     C                   ADD       1             W
068200970526     C                   MOVEL     FUDDES        FUV(W)
068300970526     C                   END
068400970526     C                   END                                                    DO WHILE
068500970526     C*
068600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
068700970526     C* ALTRIMENTI DISATTIVO F24
068800970526     C     1             DO        24            I
068900970526     C                   MOVEL     FUD(I)        FUDRIG
069000970526     C*
069100970526     C     FUDFUN        IFEQ      '24'
069200970526     C     W             IFGT      6
069300970526     C                   MOVEL     'S'           FUDATT
069400970526     C                   ELSE
069500970526     C                   MOVEL     *BLANKS       FUDATT
069600970526     C                   END                                                    W GT 8
069700970526     C                   MOVEL     FUDRIG        FUD(I)
069800970526     C                   END
069900970526     C                   END                                                    DO WHILE
070000970526     C*
070100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
070200970526     C     W             DIV       6             I
070300970526     C                   MVR                     Z
070400970526     C*
070500970526     C     Z             IFNE      0
070600970526     C     I             ADD       1             Z
070700970526     C                   ELSE
070800970526     C                   Z-ADD     I             Z                 5 0
070900970526     C                   END
071000970526     C*
071100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
071200970526     C                   ADD       1             FUNGRU
071300970526     C     FUNGRU        IFGT      Z
071400970526     C     FUNGRU        ORLT      1
071500970526     C                   Z-ADD     1             FUNGRU
071600970526     C                   END
071700970526     C*
071800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
071900970526     C     FUNGRU        IFEQ      1
072000970526     C                   MOVEL     FUV(1)        FUN001
072100970526     C                   MOVEL     FUV(2)        FUN002
072200970526     C                   MOVEL     FUV(3)        FUN003
072300970526     C                   MOVEL     FUV(4)        FUN004
072400970526     C                   MOVEL     FUV(5)        FUN005
072500970526     C                   MOVEL     FUV(6)        FUN006
072600970526     C                   END
072700970526     C*
072800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
072900970526     C     FUNGRU        IFEQ      2
073000970526     C                   MOVEL     FUV(7)        FUN001
073100970526     C                   MOVEL     FUV(8)        FUN002
073200970526     C                   MOVEL     FUV(9)        FUN003
073300970526     C                   MOVEL     FUV(10)       FUN004
073400970526     C                   MOVEL     FUV(11)       FUN005
073500970526     C                   MOVEL     FUV(12)       FUN006
073600970526     C                   END
073700970526     C*
073800970526     C* TERZO GRUPPO DI 6 FUNZIONI
073900970526     C     FUNGRU        IFEQ      3
074000970526     C                   MOVEL     FUV(13)       FUN001
074100970526     C                   MOVEL     FUV(14)       FUN002
074200970526     C                   MOVEL     FUV(15)       FUN003
074300970526     C                   MOVEL     FUV(16)       FUN004
074400970526     C                   MOVEL     FUV(17)       FUN005
074500970526     C                   MOVEL     FUV(18)       FUN006
074600970526     C                   END
074700970526     C*
074800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
074900970526     C     FUNGRU        IFEQ      4
075000970526     C                   MOVEL     FUV(19)       FUN001
075100970526     C                   MOVEL     FUV(20)       FUN002
075200970526     C                   MOVEL     FUV(21)       FUN003
075300970526     C                   MOVEL     FUV(22)       FUN004
075400970526     C                   MOVEL     *BLANKS       FUN005
075500970526     C                   MOVEL     *BLANKS       FUN006
075600970526     C                   END
075700970526     C*
075800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
075900970526     C                   MOVEL     *BLANKS       FUN007
076000970526     C                   MOVEL     *BLANKS       FUN008
076100970526     C*
076200970526     C     1             DO        24            I
076300970526     C                   MOVEL     FUD(I)        FUDRIG
076400970526     C     FUDFUN        IFEQ      '24'
076500970526     C     FUDATT        ANDEQ     'S'
076600970526     C                   MOVEL     FUDDES        FUN008
076700970526     C                   END
076800970526     C                   END                                                    DO
076900970526     C*
077000970526     C                   ENDSR
077100970526     C*------------------------------------------------------------------------*
077200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
077300970526     C*------------------------------------------------------------------------*
077400970526     C     FUNCON        BEGSR
077500970526     C*
077600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
077700970526     C                   EXSR      FUNVAR
077800970526     C*
077900970526     C* PULISCO CAMPI DI LAVORO
078000970526     C                   Z-ADD     0             I                 5 0
078100970526     C                   Z-ADD     0             W                 5 0
078200970526     C*
078300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
078400970526     C     FUNKEY        IFNE      *BLANKS
078500970526     C     1             DO        24            I
078600970526     C                   MOVEL     FUD(I)        FUDRIG
078700970526     C*
078800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
078900970526     C     FUNKEY        IFEQ      FUDFUN
079000970526     C     FUDATT        ANDEQ     'S'
079100970526     C                   Z-ADD     I             W
079200970526     C                   END
079300970526     C                   END                                                    DO WHILE
079400970526     C*
079500970526     C* FUNZIONE NON ATTIVA (ERRORE)
079600970526     C     W             IFEQ      0
079700970526     C*
079800970526     C* EMISSIONE MESSAGGIO DI ERRORE
079900970526     C                   MOVEL     MSG(1)        DSMSMS
080000970526     C*
080100970526     C                   END                                                    W=0
080200970526     C                   END                                                    FUNKEY NE *BLANKS
080300970526     C*
080400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
080500970526     C     W             IFNE      0
080600970526     C     FUNKEY        OREQ      *BLANKS
080700970526     C     FUNKEY        IFEQ      *BLANKS
080800970526     C                   EXSR      ENTGES
080900970526     C                   END
081000040818     C     FUNKEY        IFEQ      '01'
081100040818     C                   EXSR      F01GES
081200040818     C                   END
081300970526     C     FUNKEY        IFEQ      '03'
081400970526     C                   EXSR      F03GES
081500970526     C                   END
081600970526     C     FUNKEY        IFEQ      '06'
081700970526     C                   EXSR      F06GES
081800970526     C                   END
081900030822     C     FUNKEY        IFEQ      '09'
082000030822     C                   EXSR      F09GES
082100030822     C                   END
082200970526     C     FUNKEY        IFEQ      '12'
082300970526     C                   EXSR      F12GES
082400970526     C                   END
082500970526     C     FUNKEY        IFEQ      '24'
082600970526     C                   EXSR      F24GES
082700970526     C                   END
082800970526     C                   END
082900970526     C*
083000970526     C                   ENDSR
083100970526     C*------------------------------------------------------------------------*
083200970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
083300970526     C*------------------------------------------------------------------------*
083400970526     C     FUNVAR        BEGSR
083500970526     C*
083600970526     C* INIZIALIZZA VARIABILE -FUNKEY-
083700970526     C*
083800030509     C* FUNKEY=*BLANKS TCMNIFICA ENTER
083900970526     C     KEY           IFEQ      ENTER
084000970526     C                   MOVEL     *BLANKS       FUNKEY
084100970526     C                   END
084200970526     C     KEY           IFEQ      F1
084300970526     C                   MOVEL     '01'          FUNKEY
084400970526     C                   END
084500970526     C     KEY           IFEQ      F2
084600970526     C                   MOVEL     '02'          FUNKEY
084700970526     C                   END
084800970526     C     KEY           IFEQ      F3
084900970526     C                   MOVEL     '03'          FUNKEY
085000970526     C                   END
085100970526     C     KEY           IFEQ      F4
085200970526     C                   MOVEL     '04'          FUNKEY
085300970526     C                   END
085400970526     C     KEY           IFEQ      F5
085500970526     C                   MOVEL     '05'          FUNKEY
085600970526     C                   END
085700970526     C     KEY           IFEQ      F6
085800970526     C                   MOVEL     '06'          FUNKEY
085900970526     C                   END
086000970526     C     KEY           IFEQ      F7
086100970526     C                   MOVEL     '07'          FUNKEY
086200970526     C                   END
086300970526     C     KEY           IFEQ      F8
086400970526     C                   MOVEL     '08'          FUNKEY
086500970526     C                   END
086600970526     C     KEY           IFEQ      F9
086700970526     C                   MOVEL     '09'          FUNKEY
086800970526     C                   END
086900970526     C     KEY           IFEQ      F10
087000970526     C                   MOVEL     '10'          FUNKEY
087100970526     C                   END
087200970526     C     KEY           IFEQ      F11
087300970526     C                   MOVEL     '11'          FUNKEY
087400970526     C                   END
087500970526     C     KEY           IFEQ      F12
087600970526     C                   MOVEL     '12'          FUNKEY
087700970526     C                   END
087800970526     C     KEY           IFEQ      F13
087900970526     C                   MOVEL     '13'          FUNKEY
088000970526     C                   END
088100970526     C     KEY           IFEQ      F14
088200970526     C                   MOVEL     '14'          FUNKEY
088300970526     C                   END
088400970526     C     KEY           IFEQ      F15
088500970526     C                   MOVEL     '15'          FUNKEY
088600970526     C                   END
088700970526     C     KEY           IFEQ      F16
088800970526     C                   MOVEL     '16'          FUNKEY
088900970526     C                   END
089000970526     C     KEY           IFEQ      F17
089100970526     C                   MOVEL     '17'          FUNKEY
089200970526     C                   END
089300970526     C     KEY           IFEQ      F18
089400970526     C                   MOVEL     '18'          FUNKEY
089500970526     C                   END
089600970526     C     KEY           IFEQ      F19
089700970526     C                   MOVEL     '19'          FUNKEY
089800970526     C                   END
089900970526     C     KEY           IFEQ      F20
090000970526     C                   MOVEL     '20'          FUNKEY
090100970526     C                   END
090200970526     C     KEY           IFEQ      F21
090300970526     C                   MOVEL     '21'          FUNKEY
090400970526     C                   END
090500970526     C     KEY           IFEQ      F22
090600970526     C                   MOVEL     '22'          FUNKEY
090700970526     C                   END
090800970526     C     KEY           IFEQ      F23
090900970526     C                   MOVEL     '23'          FUNKEY
091000970526     C                   END
091100970526     C     KEY           IFEQ      F24
091200970526     C                   MOVEL     '24'          FUNKEY
091300970526     C                   END
091400970526     C     KEY           IFEQ      ROLLUP
091500970526     C                   MOVEL     'RU'          FUNKEY
091600970526     C                   END
091700970526     C     KEY           IFEQ      HELP
091800970526     C                   MOVEL     'HP'          FUNKEY
091900970526     C                   END
092000970526     C                   ENDSR
092100970526     C*------------------------------------------------------------------------*
092200970526     C* NOTE COSTRUZIONE SCHIERE
092300970526     C*------------------------------------------------------------------------*
092400970526     C*
092500970526     C* -MSG-MESSAGGI
092600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
092700970526     C* (NON TOCCARE MSG,1 E MSG,2)
092800970526     C*
092900970526     C* -CMD-COMANDI
093000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
093100970526     C*
093200970526     C* -OPD-OPZIONI
093300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
093400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
093500970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
093600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
093700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
093800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
093900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
094000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
094100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
094200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
094300970526     C*
094400970526     C* -FUD-FUNZIONI
094500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
094600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
094700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
094800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
094900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
095000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
095100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
095200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
095300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
095400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
095500970526     O*------------------------------------------------------------------------*
095600970526** DEC - DECODIFICHE
095700970526    MODIFICA                                                      1
095800030509VISUALIZZAZIONE                                                   2
095900031210 CANCELLAZIONE                                                    3
096000031210   RILASCIO                                                       4
0961000305095
096200970528** MSG - MESSAGGI
096300030509TRTCM4R- Tasto funzionale non gestito                                          1
096400030509TRTCM4R- Modifica effettuata                                                   2
096500030509TRTCM4R- Record non trovato                                                    3
096600030822TRTCM4R- Regola non valorizzata, impossibile visualizzare                      4
096700031210TRTCM4R- Rilascio effettuato, record pronto x l'elaborazione                   5
096800031210TRTCM4R- Eliminazione effettuata                                               6
096900031210TRTCM4R- Errori in fase di eliminazione file da IFS, F6 x forzare              7
097000031210TRTCM4R- Record di log nn è congelato, impossibile rilasciare                  8
097100031210TRTCM4R- Codice regola inesistente, il record verrà automaticamenete congelato 9
097200031210TRTCM4R- Dati insufficienti, valorizzare almeno il codice regola.              10
097300040924TRTCM4R- Record da elaborare, impossibile cambiare stato.                      11
097400040924TRTCM4R- 12                                                                    12
097500030509TRTCM4R- 13                                                                    13
097600030509TRTCM4R- 14                                                                    14
097700030509TRTCM4R- 15                                                                    15
097800030509TRTCM4R- 16                                                                    16
097900030509TRTCM4R- 17                                                                    17
09800002121118                                                                             18
09810002121119                                                                             19
09820002121120                                                                             20
098300970526** FUD - FUNZIONI
098400040818F01=Help campo     -        (S/N)=S                        funz.01¬
098500970526                   -        (S/N)=N                        funz.02¬
098600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
098700970526                   -        (S/N)=N                        funz.04¬
098800970526                   -        (S/N)=N                        funz.05¬
098900000407F06=Conferma       -        (S/N)=S                        funz.06¬
099000030822                   -        (S/N)=N                        funz.07¬
099100970527                   -        (S/N)=N                        funz.08¬
099200030822F09=Visual. regola -        (S/N)=S                        funz.09¬
099300970527                   -        (S/N)=N                        funz.10¬
099400970526                   -        (S/N)=N                        funz.11¬
099500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
099600970526                   -        (S/N)=N                        funz.13¬
099700970526                   -        (S/N)=N                        funz.14¬
099800970526                   -        (S/N)=N                        funz.15¬
099900970526                   -        (S/N)=N                        funz.16¬
100000970526                   -        (S/N)=N                        funz.17¬
100100970526                   -        (S/N)=N                        funz.18¬
100200970526                   -        (S/N)=N                        funz.19¬
100300970526                   -        (S/N)=N                        funz.20¬
100400970526                   -        (S/N)=N                        funz.21¬
100500970526                   -        (S/N)=N                        funz.22¬
100600970526                   -        (S/N)=N                        funz.23¬
100700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
