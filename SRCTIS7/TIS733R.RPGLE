000100970526     H*------------------------------------------------------------------------*
000200001020     H* ESTENSIONE ANAGRAFICA CLIENTI ABILITATI
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800001020     Ftis733d   cf   e             WORKSTN INFDS(DEVDS1)
000900001020     F                                     SFILE(s733DMS:MSGULT)
001000970527     F*------------
001100001020     Ftiabl01L  UF A E           K DISK
001200001020     Ftianv01L  IF   E           K DISK
001300090622     Fsgius     IF   E           K DISK
001400060607     Fazlin03l  IF   E           K DISK
001500970526     D*------------------------------------------------------------------------*
001600970526     D* SCHIERE
001700970526     D*------------------------------------------------------------------------*
001800970526     D*------------
001900970526     D* DECODIFICHE
002000970526     D*------------
002100991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* MESSAGGI
002400970526     D*------------
002500970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* DESCRIZIONI TASTI FUNZIONALI
002800970526     D*------------
002900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* FUNZIONI DA CARICARE A VIDEO
003200970526     D*------------
003300970526     D FUV             S             19    DIM(24)
003400970526     D*------------------------------------------------------------------------*
003500970526     D* INPUT
003600970526     D*------------------------------------------------------------------------*
003700970526     D*------------
003800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003900970526     D*------------
004000970526     D FUDDS           DS
004100970526     D  FUDDE1                 1      1
004200970526     D  FUDFUN                 2      3
004300970526     D  FUDDE2                 4     19
004400970526     D  FUDFI0                20     34
004500970526     D  FUDATT                35     35
004600970526     D  FUDFI1                36     80
004700970526     D  FUDDES                 1     19
004800970526     D  FUDRIG                 1     80
004900970526     D*------------
005000970526     D* DS GESTIONE TASTI FUNZIONALI
005100970526     D*------------
005200970526     D DSKEY         E DS
005300970526     D DEVDS1          DS
005400970526     D  KEY                  369    369
005500970526     D*------------
005600970526     D* DS "XMSG" - GESTIONE MESSAGGI
005700970526     D*------------
005800970526     D DSMSG         E DS
005900001020     D*------------
006000001020     D* DS - REPERIMENTO TABELLE
006100001020     D*------------
006200991108     D TIBS02DS      E DS
006300970528     D*------------------
006400970526     D* DS DI PROCEDURA
006500970526     D*------------
006600001020     D tis732        E DS                  EXTNAME(tis732ds)
006700060607     D ablFl1        E DS
006800060607     D                                     QUALIFIED
006900171019     D DISS          E DS                  INZ
007000970526     D*------------
007100001020     D* DS DI REPERIMENTO DATI CLIENTE
007200001020     D*------------
007300001020     DBS69DS         e ds                  extname(tibs69ds)
007400001020     DACODS          e ds                  extname(cnaco00f)
007500001020     DINDDS          e ds                  extname(cnind00f)
007600001020     DCLPDS          e ds                  extname(cnclp00f)
007700001020     DCLSDS          e ds                  extname(fncls00f)
007800001120     D*------------------
007900001120     D* DS X CONTROLLO INDIRIZZO E-MAIL
008000001120     D*------------
008100001120     D DSEMAIL       E DS
008200001020     D*------------
008300970526     D* ARCHITETTURA
008400970526     D*------------
008500970526     D KPJBA         E DS
008600010313     D*------------------
008700010313     D* VARIABILI DI WRK
008800010313     D*------------------
008900010313     D API             s              1    inz('''')
009000100503     D
009100100503     D                 ds
009200100615     D  val                          63A   INZ(' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123-
009300100615     D                                     456789abcdefghijklmnopqrstuvwxyz')
009400100615     D  valAry                        1A   DIM(63)
009500100503     D                                     OVERLAY(val)
009600970526     C*------------------------------------------------------------------------*
009700970526     C* MAIN LINES
009800970526     C*------------------------------------------------------------------------*
009900970526     C*
010000970526     C* NON CI SONO ERRORI IN ENTRATA
010100001020     C     ERRL00        IFEQ      '0'
010200970526     C*
010300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010400001020     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010700970526     C*
010800970526     C* EMETTE FORMATO VIDEO
010900970526     C                   EXSR      BUTFMR
011000970526     C*
011100970526     C* GESTIONE TASTI FUNZIONALI
011200970526     C                   EXSR      FUNCON
011300001020     C                   ENDDO
011400970526     C*
011500970526     C* CI SONO ERRORI IN ENTRATA
011600001020     C                   ELSE
011700001020     C                   MOVEL     '1'           s32err
011800001020     C                   MOVEL     DSMSMS        s32msg
011900001020     C                   ENDIF
012000970526     C* FINE PGM
012100970526     C                   SETON                                        LR
012200970526     C*------------------------------------------------------------------------*
012300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012400970526     C*------------------------------------------------------------------------*
012500970526     C     BUTFMR        BEGSR
012600970526     C*
012700970526     C* EMETTE LA TESTATA
012800001020     C                   WRITE     s733D01
012900970526     C*
013000970526     C* SPEDISCE EVENTUALE MESSAGGIO
013100970526     C                   EXSR      SNDMSG
013200970526     C*
013300970526     C* EMETTE LE FUNZIONI
013400001020     C                   WRITE     s733D02
013500970526     C*
013600970526     C* EMETTE IL VIDEO
013700001020     C                   EXFMT     s733D03
013800970526     C*
013900970526     C* REIMPOSTA FLAG DI ERRORE
014000970526     C                   MOVEL     '0'           ERRL00
014100970526     C*
014200970526     C* REIMPOSTA INDICATORI DI ERRORE
014300001024     C                   SETOFF                                       606162
014400001024     C                   SETOFF                                       636465
014500010227     C                   SETOFF                                       666780
014600060607     C                   SETOFF                                       68
014700970526     C*
014800970526     C                   ENDSR
014900970526     C*------------------------------------------------------------------------*
015000970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015100970526     C*------------------------------------------------------------------------*
015200970526     C     SNDMSG        BEGSR
015300970526     C*
015400970526     C* PULISCE IL SFILE MESSAGGI
015500970526     C                   MOVEL     '1'           *IN40
015600001020     C                   WRITE     s733DMC
015700970526     C                   MOVEL     '0'           *IN40
015800970526     C*
015900970526     C* VISUALIZZA IL MESSAGGIO
016000970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016100970526     C                   CALL      'XMSG'        XMSGPA
016200001020     C     DSMSER        IFEQ      *ZEROS
016300970526     C                   MOVEL     DSMSKE        MSGKEY
016400001020     C                   ENDIF
016500970526     C                   Z-ADD     1             MSGULT
016600001020     C                   WRITE     s733DMS
016700001020     C                   WRITE     s733DMC
016800970526     C*
016900970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017000970526     C                   MOVEL     *BLANKS       DSMSMS
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* ENTGES - GESTIONE ENTER -> INVIO
017500970526     C*------------------------------------------------------------------------*
017600970526     C     ENTGES        BEGSR
017700970526     C*
017800970526     C* CONTROLLA I CAMPI IMMESSI
017900970526     C                   EXSR      DECODI
018000970526     C*
018100970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
018200001020     C     DEPOP1        IFEQ      'O05'
018300970526     C                   MOVEL     '1'           FINL00
018400001020     C                   ENDIF
018500970526     C*
018600970526     C                   ENDSR
018700970526     C*------------------------------------------------------------------------*
018800970526     C* F03GES - GESTIONE F03 -> FINE
018900970526     C*------------------------------------------------------------------------*
019000970526     C     F03GES        BEGSR
019100970526     C*
019200970526     C* IMPOSTA "PREMUTO F03"
019300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019400001020     C                   MOVEL     '1'           s32F03                         *DI PROCEDURA
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
019900970526     C*------------------------------------------------------------------------*
020000970526     C     F06GES        BEGSR
020100970526     C*
020200970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
020300001020     C     DEPOP1        IFEQ      'F06'
020400970526     C                   EXSR      INSERI
020500001020     C                   ENDIF
020600001020     C     DEPOP1        IFEQ      'O02'
020700970526     C                   EXSR      MODIFI
020800001020     C                   ENDIF
020900010810     C     DEPOP1        IFEQ      'O08'
021000010810     C                   EXSR      MODIFIEDP
021100010810     C                   ENDIF
021200970526     C*
021300970526     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500970526     C* F12GES - GESTIONE F12 -> RITORNO
021600970526     C*------------------------------------------------------------------------*
021700970526     C     F12GES        BEGSR
021800970526     C*
021900970526     C* IMPOSTA "PREMUTO F12"
022000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022100001020     C                   MOVEL     '1'           s32F12                         *DI PROCEDURA
022200970526     C*
022300970526     C                   ENDSR
022400970526     C*------------------------------------------------------------------------*
022500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022600970526     C*------------------------------------------------------------------------*
022700970526     C     F24GES        BEGSR
022800970526     C*
022900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023000970526     C*
023100970526     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
023400970526     C*------------------------------------------------------------------------*
023500970526     C     INSERI        BEGSR
023600970526     C*
023700970526     C* CONTROLLA I CAMPI IMMESSI
023800970526     C                   EXSR      DECODI
023900970526     C*
024000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
024100001020     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024200001020     c                   EXSR      IMPabl
024300001020     C                   WRITE     tiabl000
024400001020     C                   MOVEL     MSG(2)        s32MSG                         *MESSAGGIO
024500970602     C*
024600970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
024700970602     C                   EXSR      AZZVID
024800170111     C*
024900170111     C* ASSEGNA I PARAMETRI IN INPUT A VIDEO (SE PASSATI)
025000170111     C                   EXSR      PASSPAR
025100001020     C                   ENDIF
025200970526     C*
025300970526     C                   ENDSR
025400970526     C*------------------------------------------------------------------------*
025500001116     C* MODIFI - OPERAZIONI DI MODIFICA
025600970526     C*------------------------------------------------------------------------*
025700970526     C     MODIFI        BEGSR
025800970526     C*
025900970526     C* CONTROLLA I CAMPI IMMESSI
026000970526     C                   EXSR      DECODI
026100970526     C*
026200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
026300001020     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026400001020     C                   EXSR      LETabl
026500001116     C*
026600100907     C                   MOVEL     VIDSGIANAM    alfaute
026700001116     C                   EVAL      alfaeml = VIDem1 + VIDem2
026800010726     C* EFFETTUO CONTROLLI ED ESEGUO SERIE COMANDI REMOTI SU RAGIONE SOCIALE E RESPONSABILE
026900010726     C                   Z-ADD     *zeros        pos
027000010726     C                   EVAL      pos = %scan(api:VDDksu)
027100010726     C                   DOW       pos > *zeros
027200010726     C                   EVAL      VDDksu = %replace(' ':VDDksu:pos)
027300010726     C                   Z-ADD     *zeros        pos
027400010726     C                   EVAL      pos = %scan(api:VDDksu)
027500010726     C                   ENDDO
027600010726     C*
027700010726     C                   Z-ADD     *zeros        pos
027800010726     C                   EVAL      pos = %scan(api:VIDrsp)
027900010726     C                   DOW       pos > *zeros
028000010726     C                   EVAL      VIDrsp = %replace(' ':VIDrsp:pos)
028100010726     C                   Z-ADD     *zeros        pos
028200010726     C                   EVAL      pos = %scan(api:VIDrsp)
028300010726     C                   ENDDO
028400010726     C*
028500010810     C*                  CALL(e)   'TIS730R1'
028600100906     C*                  PARM                    alfaute          40
028700010810     C*                  PARM      VIDpwd        alfapwd          20
028800010810     C*                  PARM      VDDksu        alfarag          20
028900010810     C*                  PARM      VIDrsp        alfarsp          30
029000010810     C*                  PARM      VIDsun        alfasun           9
029100010810     C*                  PARM      *blanks       esito             1
029200010810     C*                  PARM      'A'           operazione        1
029300010810     C*                  PARM                    alfaeml          78
029400010810     C*                  IF        %error or esito = '1'
029500010810     C*                  MOVEL     '1'           ERRL00
029600010810     C*                  MOVEL     MSG(10)       DSMSMS
029700010810     C*                  ELSE
029800010810     C*                  IF        esito = '2'
029900010810     C*                  MOVEL     '1'           ERRL00
030000010810     C*                  MOVEL     MSG(9)        DSMSMS
030100010810     C*                  ENDIF
030200010810     C*                  ENDIF
030300970526     C*
030400970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030500010810     C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
030600001121xxx**C*                  MOVEL     '0'           VIDinv
030700001020     C                   EXSR      IMPabl                                       *IMPOSTA I CAMPI
030800001020     C                   UPDATE    tiabl000
030900001020     C                   MOVEL     MSG(3)        s32MSG
031000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031100010810     C*                  ENDIF
031200001020     C                   ENDIF
031300970526     C*
031400970526     C                   ENDSR
031500010810     C*------------------------------------------------------------------------*
031600010810     C* MODIFIEDP - OPERAZIONI DI MODIFICA AD USO EDP
031700010810     C*------------------------------------------------------------------------*
031800010810     C     MODIFIEDP     BEGSR
031900010810     C*
032000010810     C* CONTROLLA I CAMPI IMMESSI
032100010810     C                   EXSR      DECODI
032200010810     C*
032300010810     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032400010810     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032500010810     C                   EXSR      LETabl
032600010810     C*
032700100907     C                   MOVEL     VIDSGIANAM    alfaute
032800010810     C                   EVAL      alfaeml = VIDem1 + VIDem2
032900010810     C* EFFETTUO CONTROLLI ED ESEGUO SERIE COMANDI REMOTI SU RAGIONE SOCIALE E RESPONSABILE
033000010810     C                   Z-ADD     *zeros        pos
033100010810     C                   EVAL      pos = %scan(api:VDDksu)
033200010810     C                   DOW       pos > *zeros
033300010810     C                   EVAL      VDDksu = %replace(' ':VDDksu:pos)
033400010810     C                   Z-ADD     *zeros        pos
033500010810     C                   EVAL      pos = %scan(api:VDDksu)
033600010810     C                   ENDDO
033700010810     C*
033800010810     C                   Z-ADD     *zeros        pos
033900010810     C                   EVAL      pos = %scan(api:VIDrsp)
034000010810     C                   DOW       pos > *zeros
034100010810     C                   EVAL      VIDrsp = %replace(' ':VIDrsp:pos)
034200010810     C                   Z-ADD     *zeros        pos
034300010810     C                   EVAL      pos = %scan(api:VIDrsp)
034400010810     C                   ENDDO
034500010810     C*
034600171019     C* Se sottotipo servizio è di tipo Strategi => eseguo anche comando remoto su STRATEGI
034700171019     C                   IF        §ISSSGI = 'S'
034800010810     C                   CALL(e)   'TIS730R1'
034900100906     C                   PARM                    alfaute          40
035000010810     C                   PARM      VIDpwd        alfapwd          20
035100010810     C                   PARM      VDDksu        alfarag          20
035200010810     C                   PARM      VIDrsp        alfarsp          30
035300010810     C                   PARM      VIDsun        alfasun           9
035400010810     C                   PARM      *blanks       esito             1
035500010810     C                   PARM      'A'           operazione        1
035600010810     C                   PARM                    alfaeml          78
035700010810     C                   IF        %error or esito = '1'
035800010810     C                   MOVEL     '1'           ERRL00
035900010810     C                   MOVEL     MSG(10)       DSMSMS
036000010810     C                   ELSE
036100010810     C                   IF        esito = '2'
036200010810     C                   MOVEL     '1'           ERRL00
036300010810     C                   MOVEL     MSG(9)        DSMSMS
036400010810     C                   ENDIF
036500010810     C                   ENDIF
036600171019     C                   ENDIF
036700010810     C*
036800010810     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
036900010810     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
037000010810xxx**C*                  MOVEL     '0'           VIDinv
037100010810     C                   EXSR      IMPabl                                       *IMPOSTA I CAMPI
037200010810     C                   UPDATE    tiabl000
037300010810     C                   MOVEL     MSG(3)        s32MSG
037400010810     C                   MOVEL     '1'           FINL00                         *FINE PGM
037500010810     C                   ENDIF
037600010810     C                   ENDIF
037700010810     C*
037800010810     C                   ENDSR
037900970526     C*------------------------------------------------------------------------*
038000001020     C* LETabl - LEGGE IL RECORD PERL'AGGIORNAMENTO
038100970526     C*------------------------------------------------------------------------*
038200001020     C     LETabl        BEGSR
038300970526     C*
038400001020     C                   MOVEL     s32sun        KABsun
038500001020     C                   MOVEL     s32isv        KABisv
038600001020     C                   MOVEL     s32tip        KABtip
038700090707     C     KEYabl        CHAIN     tiabl01L                           9998
038800001020     C     *IN98         IFEQ      *ON                                          *ALLOCATO
038900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
039000001020     C                   MOVEL     MSG(7)        s32MSG                         *MESSAGGIO
039100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
039200001020     C                   ENDIF
039300001020     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
039400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
039500001020     C                   MOVEL     MSG(6)        s32MSG                         *MESSAGGIO
039600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
039700001020     C                   ENDIF
039800970526     C*
039900970526     C                   ENDSR
040000001020     C*------------------------------------------------------------------------*
040100001024     C* DECODI - CONTROLLA CAMPI E DECODIFICA
040200001020     C*------------------------------------------------------------------------*
040300001020     C     DECODI        BEGSR
040400001024     C*
040500001024     C* Utente Strategi
040600001024     C     ERRL00        IFEQ      '0'
040700001024     C     VIDsun        IFNE      *blanks
040800001024     C                   ELSE
040900001024     C                   MOVEL     '1'           ERRL00
041000001024     C                   SETON                                        60
041100001024     C                   MOVEL     MSG(11)       DSMSMS
041200001024     C                   ENDIF
041300001024     C                   ENDIF
041400001020     C*
041500001020     C* Controllo del codice
041600001020     C     ERRL00        IFEQ      '0'
041700001024     C     VIDksu        IFNE      *blanks
041800001020     C     VIDksu        ANDNE     *zeros
041900001020     c                   EXSR      cntksc
042000001020     C                   ELSE
042100001020     C                   MOVEL     '1'           ERRL00
042200001020     C                   SETON                                        61
042300001024     C                   MOVEL     MSG(11)       DSMSMS
042400001020     C                   ENDIF
042500001020     C                   ENDIF
042600001020     C*
042700001020     C* Tipo servizio
042800001020     C     ERRL00        IFEQ      '0'
042900001024     C     VIDisv        IFNE      *blanks
043000001020     c                   EXSR      cntisv
043100001020     C                   ELSE
043200001020     C                   MOVEL     '1'           ERRL00
043300001020     C                   SETON                                        64
043400001024     C                   MOVEL     MSG(11)       DSMSMS
043500001020     C                   ENDIF
043600001020     C                   ENDIF
043700001024     C*
043800170503     C* Tipo abilitazione per invio
043900001024     C     ERRL00        IFEQ      '0'
044000001024     C     VIDtip        IFNE      *blanks
044100170503     c                   EXSR      cnttip
044200001024     C                   ELSE
044300001024     C                   MOVEL     '1'           ERRL00
044400001024     C                   SETON                                        65
044500001024     C                   MOVEL     MSG(11)       DSMSMS
044600001024     C                   ENDIF
044700001024     C                   ENDIF
044800001024     C*
044900001024     C* Password iniziale
045000001024     C     ERRL00        IFEQ      '0'
045100001024     C     VIDpwd        IFNE      *blanks
045200001024     C                   ELSE
045300001024     C                   MOVEL     '1'           ERRL00
045400001024     C                   SETON                                        66
045500001024     C                   MOVEL     MSG(11)       DSMSMS
045600001024     C                   ENDIF
045700001024     C                   ENDIF
045800100503     C*
045900100503     C* Password => caratteri consentiti
046000100503     C     ERRL00        IFEQ      '0'
046100100503     C                   Z-ADD     1             pos               2 0
046200100503     C                   FOR       pos = 1 to %len(%trim(VIDpwd))
046300100503     C                   IF        %lookup(%subst(%trim(VIDpwd):pos:1):valAry)
046400100503     C                             = *zeros
046500100503     C                   MOVEL     '1'           ERRL00
046600100503     C                   SETON                                        66
046700100503     C                   MOVEL     MSG(16)       DSMSMS
046800100503     C                   ENDIF
046900100503     C                   ENDFOR
047000100503     C                   ENDIF
047100001024     C*
047200010313     C* Responsabile referente
047300001024     C     ERRL00        IFEQ      '0'
047400001024     C     VIDrsp        IFNE      *blanks
047500010313     C                   Z-ADD     *zeros        pos               2 0
047600010313     C                   EVAL      pos = %scan(api:VIDrsp)
047700010313     C                   IF        pos > 0
047800010313     C                   MOVEL     '1'           ERRL00
047900010313     C                   MOVEL     MSG(14)       DSMSMS
048000010313     C                   ENDIF
048100001024     C                   ELSE
048200001024     C                   MOVEL     '1'           ERRL00
048300001024     C                   SETON                                        62
048400001024     C                   MOVEL     MSG(11)       DSMSMS
048500001024     C                   ENDIF
048600001024     C                   ENDIF
048700001024     C*
048800001024     C* E-mail
048900001024     C     ERRL00        IFEQ      '0'
049000001024     C                   IF        VIDem1+VIDem2 <> *blanks
049100001120     C* Lancio utility di controllo formale indirizzo e-mail
049200001120     C                   CLEAR                   DSEMAIL
049300001120     C                   EVAL      §EMLINDI = VIDem1+VIDem2
049400001120     C                   CALL      'XEMAIL'
049500001120     C                   PARM                    DSEMAIL
049600001120     C                   IF        §EMLERRO = '0'
049700001120     C                   EVAL      VIDem1 = %subst(§EMLINDO:1:49)
049800001120     C                   EVAL      VIDem2 = %subst(§EMLINDO:50:49)
049900001120     C                   ELSE
050000001120     C                   MOVEL     '1'           ERRL00
050100001120     C                   SETON                                        63
050200001120     C                   MOVEL     §EMLMSGO      DSMSMS
050300001120     C                   ENDIF
050400001120     C                   ELSE
050500001024     C                   MOVEL     '1'           ERRL00
050600001024     C                   SETON                                        63
050700001024     C                   MOVEL     MSG(11)       DSMSMS
050800001024     C                   ENDIF
050900001024     C                   ENDIF
051000060607     C*
051100060607     C* Lingua
051200060607     C     ERRL00        IFEQ      '0'
051300060607     c                   EXSR      cntLng
051400060607     C                   ENDIF
051500001020     C*
051600001020     C                   ENDSR
051700001020     C*------------------------------------------------------------------------*
051800001020     C* CNTKSC - CONTROLLA ESISTENZA CLIENTE
051900001020     C*------------------------------------------------------------------------*
052000001020     C     CNTKSC        BEGSR
052100001020     C*
052200001020     C                   MOVE      VIDksu        wcli              7 0
052300001020     C                   CLEAR                   BS69DS
052400001020     C                   CLEAR                   ACODS
052500001020     C                   MOVEL     KNSIF         I69SIF
052600001020     C                   Z-ADD     WCLI          I69KAC
052700001020     C                   CALL      'TIBS69R'
052800001020     C                   PARM                    BS69DS
052900001020     C                   PARM                    ACODS
053000001020     C                   PARM                    INDDS
053100001020     C                   PARM                    CLPDS
053200001020     C                   PARM                    CLSDS
053300001020     C     O69ERR        IFNE      '1'
053400001020     C                   MOVEL     ACORAG        VDDksu
053500001020     C                   ELSE
053600001020     C     VIDksu        CHAIN     tianv01l
053700001020     C                   IF        %found
053800001020     C                   MOVEL     ANVDES        VDDksu
053900001020     C                   ELSE
054000001020     C                   MOVEL     *ALL'*'       VDDksu
054100001020     C                   MOVE      '1'           errl00
054200001024     C                   MOVEL     MSG(12)       dsmsms
054300001020     C                   ENDIF
054400001020     C                   ENDIF
054500001020     C*
054600001020     C                   ENDSR
054700001020     C*------------------------------------------------------------------------*
054800001020     C* CNTISV - CONTROLLA TIPO SERVIZIO
054900001020     C*------------------------------------------------------------------------*
055000001020     C     CNTISV        BEGSR
055100001020     C*
055200001020     C                   CLEAR                   TIBS02DS
055300001020     C*
055400001020     C     '?'           SCAN      VIDisv                                 30
055500001020     C*  Ricerca
055600001020     C                   eval      t02tla = 'L'
055700001020     C                   IF        *IN30 = *ON
055800001020     C                   MOVEL     'R'           t02mod
055900001020     C                   ELSE
056000001020     C                   MOVEL     'C'           t02mod
056100001020     C                   MOVEL(p)  vidisv        t02ke1
056200001020     C                   ENDIF
056300001020     C                   MOVEL     'ISV'         t02cod
056400001020     C                   MOVEL     KNSIF         t02sif
056500001020     C                   CALL      'TIBS02R'
056600001020     C                   PARM                    KPJBA
056700001020     C                   PARM                    TIBS02DS
056800001020     C*
056900001020     C                   IF        T02err = *blanks
057000001020     C                   MOVEL     T02ke1        VIDisv
057100001020     C                   MOVEL     T02uni        VDDisv
057200001020     C                   ELSE
057300001020     C                   MOVEL     '1'           errl00
057400001020     C                   SETON                                        64
057500001024     C                   MOVEL     MSG(13)       DSMSMS
057600001020     C                   ENDIF
057700001020     C*
057800001020     C                   ENDSR
057900170503     C*------------------------------------------------------------------------*
058000170503     C* CNTTIP - CONTROLLA TIPO ABILITAZIONE
058100170503     C*------------------------------------------------------------------------*
058200170503     C     CNTTIP        BEGSR
058300170503     C*
058400170503     C                   CLEAR                   TIBS02DS
058500171019     C                   CLEAR                   DISS
058600170503     C*
058700170503     C     '?'           SCAN      VIDtip                                 30
058800170503     C*  Ricerca
058900170503     C                   eval      t02tla = 'L'
059000170503     C                   IF        *IN30 = *ON
059100170503     C                   MOVEL     'R'           t02mod
059200170503     C                   ELSE
059300170503     C                   MOVEL     'C'           t02mod
059400170503     C                   MOVEL(p)  vidtip        t02ke1
059500170503     C                   ENDIF
059600170503     C                   MOVEL     'ISS'         t02cod
059700170503     C                   MOVEL     KNSIF         t02sif
059800170503     C                   CALL      'TIBS02R'
059900170503     C                   PARM                    KPJBA
060000170503     C                   PARM                    TIBS02DS
060100170503     C*
060200170503     C                   IF        T02err = *blanks
060300170503     C                   MOVEL     T02ke1        VIDtip
060400171019     C                   MOVEL     T02uni        VDDtip
060500171019     C                   EVAL      DISS = T02UNI
060600170503     C                   ELSE
060700170503     C                   MOVEL     '1'           errl00
060800170503     C                   SETON                                        65
060900170503     C                   MOVEL     MSG(13)       DSMSMS
061000170503     C                   ENDIF
061100170503     C*
061200170503     C                   ENDSR
061300060607     C*------------------------------------------------------------------------*
061400060607     C* CNTLNG - CONTROLLA LINGUA
061500060607     C*------------------------------------------------------------------------*
061600060607     C     CNTLNG        BEGSR
061700060607     C*
061800060925     C                   CLEAR                   vidLngDes
061900060925     C                   IF        vidLng <> *BLANK
062000060607     C     vidLng        CHAIN     azlin03l
062100060607     C                   IF        %FOUND
062200060607     C                   EVAL      vidLngDes = linDesIta
062300060607     C                   ELSE
062400060607     C                   MOVEL     '1'           errl00
062500060607     C                   SETON                                        68
062600060607     C                   MOVEL     MSG(15)       DSMSMS
062700060607     C                   ENDIF
062800060925     C                   ENDIF
062900060607     C*
063000060607     C                   ENDSR
063100970526     C*------------------------------------------------------------------------*
063200001020     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
063300970526     C*------------------------------------------------------------------------*
063400970526     C     IREVID        BEGSR
063500001020     C*
063600001020     C                   MOVEL     ablsun        VIDsun
063700010625     C                   MOVEL     *all'0'       VIDKSU
063800010625     C                   MOVE      ablute        VIDksu
063900001020     C                   MOVEL     ablisv        VIDisv
064000001020     C                   MOVEL     ablpwd        VIDpwd
064100001020     C                   MOVEL     ablrsp        VIDrsp
064200001020     C                   EVAL      VIDem1 = %SUBST(ABLEML:1:50)
064300001020     C                   EVAL      VIDem2 = %SUBST(ABLEML:50:50)
064400001020     C                   MOVEL     abltip        VIDtip
064500001020     C                   MOVEL     ablinv        VIDinv
064600010227     C                   MOVEL     abltel        VIDtel
064700010227     C                   MOVEL     ablfax        VIDfax
064800010227     C                   Z-ADD     abldep        VIDdep
064900060607     C                   EVAL      vidLng = ablFl1.dftVasLang
065000090622     C*
065100090622     C* Reperisco l'Access Name presente sul file utenti di Strategi
065200090622     C     VIDsun        chain     SGIUS
065300090622     C                   if        %found(SGIUS)
065400090622     C                   eval      VIDsgianam = USANAM
065500090622     C                   else
065600090622     C                   eval      VIDsgianam = *blanks
065700090622     C                   endif
065800970527     C*
065900970526     C                   ENDSR
066000970526     C*------------------------------------------------------------------------*
066100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
066200970526     C*------------------------------------------------------------------------*
066300970526     C     AZZVID        BEGSR
066400001020     C*
066500001020     C                   CLEAR                   S733D03
066600170111     C                   MOVEL     '0'           VIDinv
066700970526     C*
066800970526     C                   ENDSR
066900170111     C*------------------------------------------------------------------------*
067000170111     C* PASSPAR - ASSEGNA I PARAMETRI ININPUT A VIDEO (SE PASSATI)
067100170111     C*------------------------------------------------------------------------*
067200170111     C     PASSPAR       BEGSR
067300170111     C*
067400170111     C                   EVAL      VIDSUN = S32SUN
067500170111     C                   EVAL      VIDISV = S32ISV
067600170111     C                   EVAL      VIDTIP = S32TIP
067700170111     C                   EVAL      VIDKSU = S32KSU
067800170111     C*
067900170111     C                   ENDSR
068000970526     C*------------------------------------------------------------------------*
068100001024     C* IMPabl - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
068200970526     C*------------------------------------------------------------------------*
068300001020     C     IMPabl        BEGSR
068400970526     C*
068500001020     C                   MOVEL     VIDsun        ablsun
068600001020     C                   MOVE(p)   VIDksu        ablute
068700001020     C                   MOVEL     VIDisv        ablisv
068800001020     C                   MOVEL     VIDpwd        ablpwd
068900001020     C                   MOVEL     VIDrsp        ablrsp
069000001020     C                   EVAL      ableml = VIDem1 + VIDem2
069100001020     C                   MOVEL     VIDtip        abltip
069200001020     C                   MOVEL     VIDinv        ablinv
069300010227     C                   MOVEL     VIDtel        abltel
069400010227     C                   MOVEL     VIDfax        ablfax
069500010227     C                   Z-ADD     VIDdep        abldep
069600060607     C                   EVAL      ablFl1.dftVasLang = vidLng
069700970526     C*
069800970526     C                   ENDSR
069900970526     C*------------------------------------------------------------------------*
070000970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
070100970526     C*------------------------------------------------------------------------*
070200970526     C     IMPFUN        BEGSR
070300970526     C*
070400970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
070500001020     C     DEPOP1        IFEQ      'O05'
070600970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
070700970526     C                   MOVEL     'N'           FUDATT
070800970526     C                   MOVEL     FUDRIG        FUD(6)
070900001020     C                   ENDIF
071000970526     C*
071100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
071200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
071300970526     C                   EXSR      FUNGES
071400970526     C*
071500970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
071600970526     C*---
071700970526     C* 90 -> PROTEZIONE CHIAVE
071800970526     C* 91 -> PROTEZIONE TOTALE
071900970526     C*---
072000001020     C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
072100001020     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
072200970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
072300970526     C                   SETOFF                                       91
072400001020     C                   ENDIF
072500001020     C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
072600001020     C     DEPOP1        oreq      'O01'                                        *MODIFICA
072700970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
072800970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
072900001020     C                   ENDIF
073000001020     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
073100970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
073200970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
073300970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
073400970526     C                   SETON                                        91
073500001020     C                   ENDIF
073600001020     C                   ELSE                                                   *I N T E R R O G A
073700970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
073800970526     C                   SETON                                        91
073900001020     C                   ENDIF
074000970526     C*
074100970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
074200001020     C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
074300001020     C                   MOVEL     s32sun        KABsun
074400001020     C                   MOVEL     s32isv        KABisv
074500001020     C                   MOVEL     s32tip        KABtip
074600090707     C     KEYabl        CHAIN(n)  tiabl01L                           9998
074700001020     C     *IN99         IFEQ      *ON
074800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
074900970526     C                   MOVEL     MSG(6)        DSMSMS
075000001020     C                   ELSE
075100970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
075200001020     C     DEPOP1        IFEQ      'O05'
075300001020     C     DEPOP1        OREQ      'O02'
075400980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
075500980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
075600001020     C                   ENDIF
075700001020     C                   ENDIF
075800001020     C                   ELSE                                                   *INSERIMENTO
075900970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
076000170111     C                   EXSR      PASSPAR                                      *PASSAGGIO PARAM.IN
076100001020     C                   ENDIF
076200970526     C*
076300970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
076400001020     C     DEPOP1        IFEQ      'O02'
076500001020     C                   MOVEL     DEC(1)        VIDFUN
076600001020     C                   ENDIF
076700001020     C     DEPOP1        IFEQ      'O03'
076800991014     C                   MOVEL     DEC(6)        VIDFUN
076900001020     C                   ENDIF
077000001020     C     DEPOP1        IFEQ      'O04'
077100970526     C                   MOVEL     DEC(2)        VIDFUN
077200001020     C                   ENDIF
077300001020     C     DEPOP1        IFEQ      'O05'
077400970526     C                   MOVEL     DEC(3)        VIDFUN
077500001020     C                   ENDIF
077600001020     C     DEPOP1        IFEQ      'O07'
077700970526     C                   MOVEL     DEC(4)        VIDFUN
077800001020     C                   ENDIF
077900001020     C     DEPOP1        IFEQ      'F06'
078000970526     C                   MOVEL     DEC(5)        VIDFUN
078100001020     C                   ENDIF
078200970526     C*
078300970526     C                   ENDSR
078400970526     C*------------------------------------------------------------------------*
078500001020     C* *INZSR - OPERAZIONI INIZIALI
078600970526     C*------------------------------------------------------------------------*
078700970526     C     *INZSR        BEGSR
078800970526     C*---
078900970526     C* RICEVIMENTO PARAMETRI
079000970526     C*---
079100970526     C     *ENTRY        PLIST
079200970612     C                   PARM                    KPJBA
079300001020     C                   MOVEL     kpjbu         tis732
079400970526     C*
079500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
079600970526     C     XMSGPA        PLIST
079700970526     C                   PARM                    DSMSMS
079800970526     C                   PARM                    DSMSPG
079900970526     C                   PARM                    DSMSKE
080000970526     C                   PARM                    DSMSER
080100970526     C*
080200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
080300970526     C                   CALL      'XKEY'
080400970526     C                   PARM                    DSKEY
080500970526     C*---
080600970526     C* VARIABILI NON RIFERITE AL DATA BASE
080700970526     C*---
080800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
080900001020     C     *LIKE         DEFINE    s32OP0        DEPOP0
081000001020     C     *LIKE         DEFINE    s32OP1        DEPOP1
081100001020     C     *LIKE         DEFINE    s32OP0        WOP0
081200001020     C     *LIKE         DEFINE    s32OP1        WOP1
081300970526     C*---
081400970526     C* VARIABILI DI CONTROLLO
081500970526     C*---
081600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
081700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
081800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
081900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
082000970526     C*
082100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
082200001020     C                   MOVEL     'TIS733R'     DSMSPG
082300001020     C                   MOVEL     'TIS733R'     MSGQUE
082400970526     C                   MOVEL     *BLANKS       MSGKEY
082500970526     C                   MOVEL     *BLANKS       DSMSMS
082600970526     C*---
082700970526     C* VARIABILI RIFERITE AL DATA BASE
082800970526     C*---
082900001020     C     *LIKE         DEFINE    ablsun        KABsun
083000001020     C     *LIKE         DEFINE    ablisv        KABisv
083100001020     C     *LIKE         DEFINE    abltip        KABtip
083200970526     C*---
083300970526     C* CHIAVI DI LETTURA
083400970526     C*---
083500001020     C* LETTURA tiabl01L - COMPLETA
083600001020     C     KEYabl        KLIST
083700001020     C                   KFLD                    KABsun
083800001020     C                   KFLD                    KABisv
083900001020     C                   KFLD                    KABtip
084000970526     C*
084100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
084200001020     C                   MOVEL     s32OP0        DEPOP0                         *LIVELLO PROCEDURA
084300001020     C                   MOVEL     s32OP1        DEPOP1                         *LIVELLO PROGRAMMA
084400001020     C*
084500001020     C* TESTO SUBITO LE OPZIONI DI ENTRATA
084600010810     C                   SETOFF                                       505152
084700001020     C                   IF        DEPOP1 = 'O02'
084800010810     C                   SETON                                        5052
084900001020     C                   ELSE
085000001020     C                   IF        DEPOP1 = 'O05'
085100001020     C                   SETON                                        51
085200001020     C                   ENDIF
085300001020     C                   ENDIF
085400010227     C*
085500010227     C* ED ANCHE I PARAMETRI DI ENTRATA
085600010227     C                   IF        s32ISV = 'TD'
085700010227     C                   SETON                                        80
085800010227     C                   ENDIF
085900970526     C*
086000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
086100970526     C                   EXSR      IMPFUN
086200970526     C*
086300970526     C                   ENDSR
086400970526     C*------------------------------------------------------------------------*
086500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
086600970526     C*------------------------------------------------------------------------*
086700970526     C     FUNGES        BEGSR
086800970526     C*
086900970526     C* PULISCO CAMPI DI LAVORO
087000970526     C                   Z-ADD     0             W                 5 0
087100970526     C*
087200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
087300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
087400970526     C     1             DO        24            I
087500970526     C                   MOVEL     FUD(I)        FUDRIG
087600970526     C*
087700970526     C* PULIZIA FUNZIONI PRECEDENTI
087800970526     C                   MOVEL     *BLANKS       FUV(I)
087900970526     C*
088000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
088100970526     C* (SALTA F24)
088200970526     C     FUDATT        IFEQ      'S'
088300970526     C     FUDFUN        ANDNE     '24'
088400970526     C                   ADD       1             W
088500970526     C                   MOVEL     FUDDES        FUV(W)
088600970526     C                   END
088700970526     C                   END                                                    DO WHILE
088800970526     C*
088900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
089000970526     C* ALTRIMENTI DISATTIVO F24
089100970526     C     1             DO        24            I
089200970526     C                   MOVEL     FUD(I)        FUDRIG
089300970526     C*
089400970526     C     FUDFUN        IFEQ      '24'
089500970526     C     W             IFGT      6
089600970526     C                   MOVEL     'S'           FUDATT
089700970526     C                   ELSE
089800970526     C                   MOVEL     *BLANKS       FUDATT
089900970526     C                   END                                                    W GT 8
090000970526     C                   MOVEL     FUDRIG        FUD(I)
090100970526     C                   END
090200970526     C                   END                                                    DO WHILE
090300970526     C*
090400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
090500970526     C     W             DIV       6             I
090600970526     C                   MVR                     Z
090700970526     C*
090800970526     C     Z             IFNE      0
090900970526     C     I             ADD       1             Z
091000970526     C                   ELSE
091100970526     C                   Z-ADD     I             Z                 5 0
091200970526     C                   END
091300970526     C*
091400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
091500970526     C                   ADD       1             FUNGRU
091600970526     C     FUNGRU        IFGT      Z
091700970526     C     FUNGRU        ORLT      1
091800970526     C                   Z-ADD     1             FUNGRU
091900970526     C                   END
092000970526     C*
092100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
092200970526     C     FUNGRU        IFEQ      1
092300970526     C                   MOVEL     FUV(1)        FUN001
092400970526     C                   MOVEL     FUV(2)        FUN002
092500970526     C                   MOVEL     FUV(3)        FUN003
092600970526     C                   MOVEL     FUV(4)        FUN004
092700970526     C                   MOVEL     FUV(5)        FUN005
092800970526     C                   MOVEL     FUV(6)        FUN006
092900970526     C                   END
093000970526     C*
093100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
093200970526     C     FUNGRU        IFEQ      2
093300970526     C                   MOVEL     FUV(7)        FUN001
093400970526     C                   MOVEL     FUV(8)        FUN002
093500970526     C                   MOVEL     FUV(9)        FUN003
093600970526     C                   MOVEL     FUV(10)       FUN004
093700970526     C                   MOVEL     FUV(11)       FUN005
093800970526     C                   MOVEL     FUV(12)       FUN006
093900970526     C                   END
094000970526     C*
094100970526     C* TERZO GRUPPO DI 6 FUNZIONI
094200970526     C     FUNGRU        IFEQ      3
094300970526     C                   MOVEL     FUV(13)       FUN001
094400970526     C                   MOVEL     FUV(14)       FUN002
094500970526     C                   MOVEL     FUV(15)       FUN003
094600970526     C                   MOVEL     FUV(16)       FUN004
094700970526     C                   MOVEL     FUV(17)       FUN005
094800970526     C                   MOVEL     FUV(18)       FUN006
094900970526     C                   END
095000970526     C*
095100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
095200970526     C     FUNGRU        IFEQ      4
095300970526     C                   MOVEL     FUV(19)       FUN001
095400970526     C                   MOVEL     FUV(20)       FUN002
095500970526     C                   MOVEL     FUV(21)       FUN003
095600970526     C                   MOVEL     FUV(22)       FUN004
095700970526     C                   MOVEL     *BLANKS       FUN005
095800970526     C                   MOVEL     *BLANKS       FUN006
095900970526     C                   END
096000970526     C*
096100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
096200970526     C                   MOVEL     *BLANKS       FUN007
096300970526     C                   MOVEL     *BLANKS       FUN008
096400970526     C*
096500970526     C     1             DO        24            I
096600970526     C                   MOVEL     FUD(I)        FUDRIG
096700970526     C     FUDFUN        IFEQ      '24'
096800970526     C     FUDATT        ANDEQ     'S'
096900970526     C                   MOVEL     FUDDES        FUN008
097000970526     C                   END
097100970526     C                   END                                                    DO
097200970526     C*
097300970526     C                   ENDSR
097400970526     C*------------------------------------------------------------------------*
097500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
097600970526     C*------------------------------------------------------------------------*
097700970526     C     FUNCON        BEGSR
097800970526     C*
097900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
098000970526     C                   EXSR      FUNVAR
098100970526     C*
098200970526     C* PULISCO CAMPI DI LAVORO
098300970526     C                   Z-ADD     0             I                 5 0
098400970526     C                   Z-ADD     0             W                 5 0
098500970526     C*
098600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
098700970526     C     FUNKEY        IFNE      *BLANKS
098800970526     C     1             DO        24            I
098900970526     C                   MOVEL     FUD(I)        FUDRIG
099000970526     C*
099100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
099200970526     C     FUNKEY        IFEQ      FUDFUN
099300970526     C     FUDATT        ANDEQ     'S'
099400970526     C                   Z-ADD     I             W
099500970526     C                   END
099600970526     C                   END                                                    DO WHILE
099700970526     C*
099800970526     C* FUNZIONE NON ATTIVA (ERRORE)
099900970526     C     W             IFEQ      0
100000970526     C*
100100970526     C* EMISSIONE MESSAGGIO DI ERRORE
100200970526     C                   MOVEL     MSG(1)        DSMSMS
100300970526     C*
100400970526     C                   END                                                    W=0
100500970526     C                   END                                                    FUNKEY NE *BLANKS
100600970526     C*
100700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
100800970526     C     W             IFNE      0
100900970526     C     FUNKEY        OREQ      *BLANKS
101000970526     C     FUNKEY        IFEQ      *BLANKS
101100970526     C                   EXSR      ENTGES
101200970526     C                   END
101300970526     C     FUNKEY        IFEQ      '03'
101400970526     C                   EXSR      F03GES
101500970526     C                   END
101600970526     C     FUNKEY        IFEQ      '06'
101700970526     C                   EXSR      F06GES
101800970526     C                   END
101900970526     C     FUNKEY        IFEQ      '12'
102000970526     C                   EXSR      F12GES
102100970526     C                   END
102200970526     C     FUNKEY        IFEQ      '24'
102300970526     C                   EXSR      F24GES
102400970526     C                   END
102500970526     C                   END
102600970526     C*
102700970526     C                   ENDSR
102800970526     C*------------------------------------------------------------------------*
102900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
103000970526     C*------------------------------------------------------------------------*
103100970526     C     FUNVAR        BEGSR
103200970526     C*
103300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
103400970526     C*
103500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
103600970526     C     KEY           IFEQ      ENTER
103700970526     C                   MOVEL     *BLANKS       FUNKEY
103800970526     C                   END
103900970526     C     KEY           IFEQ      F1
104000970526     C                   MOVEL     '01'          FUNKEY
104100970526     C                   END
104200970526     C     KEY           IFEQ      F2
104300970526     C                   MOVEL     '02'          FUNKEY
104400970526     C                   END
104500970526     C     KEY           IFEQ      F3
104600970526     C                   MOVEL     '03'          FUNKEY
104700970526     C                   END
104800970526     C     KEY           IFEQ      F4
104900970526     C                   MOVEL     '04'          FUNKEY
105000970526     C                   END
105100970526     C     KEY           IFEQ      F5
105200970526     C                   MOVEL     '05'          FUNKEY
105300970526     C                   END
105400970526     C     KEY           IFEQ      F6
105500970526     C                   MOVEL     '06'          FUNKEY
105600970526     C                   END
105700970526     C     KEY           IFEQ      F7
105800970526     C                   MOVEL     '07'          FUNKEY
105900970526     C                   END
106000970526     C     KEY           IFEQ      F8
106100970526     C                   MOVEL     '08'          FUNKEY
106200970526     C                   END
106300970526     C     KEY           IFEQ      F9
106400970526     C                   MOVEL     '09'          FUNKEY
106500970526     C                   END
106600970526     C     KEY           IFEQ      F10
106700970526     C                   MOVEL     '10'          FUNKEY
106800970526     C                   END
106900970526     C     KEY           IFEQ      F11
107000970526     C                   MOVEL     '11'          FUNKEY
107100970526     C                   END
107200970526     C     KEY           IFEQ      F12
107300970526     C                   MOVEL     '12'          FUNKEY
107400970526     C                   END
107500970526     C     KEY           IFEQ      F13
107600970526     C                   MOVEL     '13'          FUNKEY
107700970526     C                   END
107800970526     C     KEY           IFEQ      F14
107900970526     C                   MOVEL     '14'          FUNKEY
108000970526     C                   END
108100970526     C     KEY           IFEQ      F15
108200970526     C                   MOVEL     '15'          FUNKEY
108300970526     C                   END
108400970526     C     KEY           IFEQ      F16
108500970526     C                   MOVEL     '16'          FUNKEY
108600970526     C                   END
108700970526     C     KEY           IFEQ      F17
108800970526     C                   MOVEL     '17'          FUNKEY
108900970526     C                   END
109000970526     C     KEY           IFEQ      F18
109100970526     C                   MOVEL     '18'          FUNKEY
109200970526     C                   END
109300970526     C     KEY           IFEQ      F19
109400970526     C                   MOVEL     '19'          FUNKEY
109500970526     C                   END
109600970526     C     KEY           IFEQ      F20
109700970526     C                   MOVEL     '20'          FUNKEY
109800970526     C                   END
109900970526     C     KEY           IFEQ      F21
110000970526     C                   MOVEL     '21'          FUNKEY
110100970526     C                   END
110200970526     C     KEY           IFEQ      F22
110300970526     C                   MOVEL     '22'          FUNKEY
110400970526     C                   END
110500970526     C     KEY           IFEQ      F23
110600970526     C                   MOVEL     '23'          FUNKEY
110700970526     C                   END
110800970526     C     KEY           IFEQ      F24
110900970526     C                   MOVEL     '24'          FUNKEY
111000970526     C                   END
111100970526     C     KEY           IFEQ      ROLLUP
111200970526     C                   MOVEL     'RU'          FUNKEY
111300970526     C                   END
111400970526     C     KEY           IFEQ      HELP
111500970526     C                   MOVEL     'HP'          FUNKEY
111600970526     C                   END
111700970526     C                   ENDSR
111800970526     C*------------------------------------------------------------------------*
111900970526     C* NOTE COSTRUZIONE SCHIERE
112000970526     C*------------------------------------------------------------------------*
112100970526     C*
112200970526     C* -MSG-MESSAGGI
112300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
112400970526     C* (NON TOCCARE MSG,1 E MSG,2)
112500970526     C*
112600970526     C* -CMD-COMANDI
112700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
112800970526     C*
112900970526     C* -OPD-OPZIONI
113000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
113100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
113200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
113300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
113400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
113500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
113600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
113700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
113800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
113900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
114000970526     C*
114100970526     C* -FUD-FUNZIONI
114200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
114300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
114400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
114500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
114600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
114700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
114800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
114900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
115000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
115100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
115200970526     O*------------------------------------------------------------------------*
115300970526** DEC - DECODIFICHE
115400970526    MODIFICA                                                      1
115500970526  ANNULLAMENTO                                                    2
115600970526VISUALIZZAZIONE                                                   3
115700970526   RIPRISTINO                                                     4
115800970526  INSERIMENTO                                                     5
115900991014     COPIA                                                        6
116000970526** MSG - MESSAGGI
116100001020TIS733R- Tasto funzionale non gestito                                          1
116200001020TIS733R- Inserimento effettuato                                                2
116300001020TIS733R- Modifica effettuata                                                   3
116400001020TIS733R- Annullamento effettuato                                               4
116500001020TIS733R- Ripristino effettuato                                                 5
116600001020TIS733R- Richiesta funzione per record inesistente                             6
116700001020TIS733R- Record momentaneamente allocato: riprovare più tardi                  7
116800001020TIS733R- Richiesta funzione per record esistente                               8
116900001116TIS733R- Cliente non iscritto in STRATEGI                                      9
117000001116TIS733R- Possibile errore remoto o di comunicazione                            10
117100001024TIS733R- Campo obbligatorio.                                                   11
117200001024TIS733R- Codice cliente errato o inesistente.                                  12
117300001024TIS733R- Tipo servizio errato o inesistente.                                   13
117400010313TIS733R- Presenti caratteri non validi (togliere gli apici).                   14
117500060607TIS733R- Lingua non ammessa per il sito VAS.                                   15
117600100503TIS733R- Caratteri non validi nella password. Inserire unicamente: A-Z  0-9    16
117700001020TIS733R-                                                                       17
117800001020TIS733R-                                                                       18
117900001020TIS733R-                                                                       19
118000001020TIS733R-                                                                       20
118100970526** FUD - FUNZIONI
118200970526                   -        (S/N)=N                        funz.01¬
118300970526                   -        (S/N)=N                        funz.02¬
118400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
118500970526                   -        (S/N)=N                        funz.04¬
118600970526                   -        (S/N)=N                        funz.05¬
118700970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
118800970527                   -        (S/N)=N                        funz.07¬
118900970527                   -        (S/N)=N                        funz.08¬
119000970527                   -        (S/N)=N                        funz.09¬
119100970527                   -        (S/N)=N                        funz.10¬
119200970526                   -        (S/N)=N                        funz.11¬
119300970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
119400970526                   -        (S/N)=N                        funz.13¬
119500970526                   -        (S/N)=N                        funz.14¬
119600970526                   -        (S/N)=N                        funz.15¬
119700970526                   -        (S/N)=N                        funz.16¬
119800970526                   -        (S/N)=N                        funz.17¬
119900970526                   -        (S/N)=N                        funz.18¬
120000970526                   -        (S/N)=N                        funz.19¬
120100970526                   -        (S/N)=N                        funz.20¬
120200970526                   -        (S/N)=N                        funz.21¬
120300970526                   -        (S/N)=N                        funz.22¬
120400970526                   -        (S/N)=N                        funz.23¬
120500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
