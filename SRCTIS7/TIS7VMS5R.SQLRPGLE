000100970603     H*--------------------------------------------------------------------------------------------*
000200970603     H DECEDIT('0,') DATEDIT(*DMY.)
000300970603     F*--------------------------------------------------------------------------------------------*
000400970603     F* DATA BASE
000500970603     F*--------------------------------------------------------------------------------------------*
000600160927     FTIS7VMS5D CF   E             WORKSTN INFDS(DEVDS1)
000700160927     F                                     SFILE(S7V5DVM:MSGULT)
000800160715     FTIVMS01I  IF   E           K DISK    rename(TIVMS000:TIVMS01)
000900970603     D*--------------------------------------------------------------------------------------------*
001000970603     D* SCHIERE
001100970603     D*--------------------------------------------------------------------------------------------*
001200970603     D*-------------------
001300970603     D* MESSAGGI E FUNZIONI
001400970603     D*-------------------
001500970603     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001600970603     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001700970603     D*-------------------
001800970603     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001900970603     D*-------------------
002000970603     D FFU             S              6    DIM(31)                              *FUNZIONE
002100970603     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002200970603     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002300970603     D*-------------------
002400970603     D* TABELLA DESCRIZIONI A VIDEO
002500970603     D*-------------------
002600970603     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002700970603     D*-------------------
002800970603     D* TABELLA TASTI FUNZIONALI PREMUTI
002900970603     D*-------------------
003000970603     D KEYS            S              1    DIM(40)
003100970603     D*--------------------------------------------------------------------------------------------*
003200970603     D* INPUT
003300970603     D*--------------------------------------------------------------------------------------------*
003400970603     D*-------------------
003500970603     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003600970603     D*-------------------
003700970603     D DSFUN         E DS
003800970603     D  FUNRIG                 1     40
003900970603     D*-------------------
004000970603     D* DS PER GESTIONE TASTI FUNZIONALI
004100970603     D*-------------------
004200970603     D DSKEYS        E DS
004300970603     D  KEYRIG                 1     40
004400970603     D*-------------------
004500970603     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004600970603     D*-------------------
004700970603     D                SDS
004800970603     D  USER                 254    263
004900970603     D*-------------------
005000970603     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005100970603     D*-------------------
005200970603     D DEVDS1          DS
005300970603     D  KEY                  369    369
005400970603     D  CURSOR               370    371
005500970603     D*---------------------
005600970603     D* ARCHITETTURA
005700970603     D*---------------------
005800970603     D KPJBA         E DS
005900970603     D*---------------------
006000970603     D* DS ESTERNA DI PROCEDURA
006100970603     D*---------------------
006200160927     D TIS7VMS5DS    E DS
006300970603     D*-------------------
006400020919     D* DS REPERIMENTO DATI UTENTE
006500970603     D*-------------------
006600020919     D TIBS34DS      E DS                                                       *Profili utente
006700020919     D DDATIUTE      E DS                                                       *Dati utente
006800020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006900970603     D*-------------------
007000970603     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007100970603     D*-------------------
007200970603     D DSMSG         E DS
007300970603     D*------------------
007400970603     D* DS "XSRDA8" - CONTROLLA DATA (8)
007500970603     D*------------------
007600970603     D WLBDA8          DS
007700970603     D  G08DAT                 1      8  0
007800970603     D  G08INV                 9     16  0
007900970603     D  G08ERR                17     17
008000970603     D  G08TGI                18     22  0
008100160715     D*------------
008200160715     D* campi tipo data per le date a video
008300160715     D DDEISO          s               d
008400160715     D DSCISO          s               d
008500160927     D*------------
008600160927     D* campi per l'SQL
008700160927     d wSQL            s           2048    inz  varying
008800970603     C*--------------------------------------------------------------------------------------------*
008900970603     C* MAIN LINE
009000970603     C*--------------------------------------------------------------------------------------------*
009100970603     C*
009200970611     C* SE NON CI SONO ERRORI IN ENTRATA
009300970611IF  1C     $FINE         IFEQ      '0'
009400160927     C                   WRITE     S7V5DVT                                      *TESTATA
009500970611DO  2C     $FINE         DOUEQ     '1'
009600970603     C*
009700970603     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009800970611IF  3C     $GES          IFEQ      'V1'
009900970603     C                   EXSR      GESV1
010000970611E   3C                   ENDIF
010100970603     C*
010200970611E   2C                   ENDDO
010300970603     C*
010400970611IF  2C     $F03          IFEQ      '1'
010500160927     C                   MOVEL     '1'           VMS5F03
010600970611E   2C                   ENDIF
010700970611E   1C                   ENDIF
010800970603     C* FINE PGM
010900970603     C                   SETON                                        LR
011000970603     C*--------------------------------------------------------------------------------------------*
011100970603     C* GESV1 - GESTIONE VIDEO UNO
011200970603     C*--------------------------------------------------------------------------------------------*
011300970603     C     GESV1         BEGSR
011400970603     C*
011500970603IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
011600970603     C                   EXSR      CARV1                                        *CARICA VIDEO
011700970603E   1C                   ENDIF
011800970603     C*
011900160902     C* QUESTA TECNICA NON FUNZIONA NELLE FINESTRE!
012000160902IF  1C*    MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012100160902     C*                  MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012200160902     C*                  EXSR      CARMSG                                       *CARICA MESSAGGIO
012300160902E   1C*                  ENDIF
012400160902     C*                  EXSR      OUTMSG                                       *EMETTE MESSAGGI
012500970603     C*
012600160927     C                   EXFMT     S7V5DV1                                      *PARAMETRI
012700160902     C                   MOVEL     *blank        V1MSG                          pulisco errmsg
012800970603     C*
012900970603     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013000970603     C*
013100970603IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013200970603SL  2C                   SELECT
013300970603     C     KEY           WHENEQ    ENTER
013400970603     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013500970603     C     KEY           WHENEQ    F6
013600970603     C                   EXSR      F06GES
013700970603     C                   OTHER
013800970603     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013900970603E   2C                   ENDSL
014000970603E   1C                   ENDIF
014100970603     C*
014200970603     C                   ENDSR
014300970603     C*--------------------------------------------------------------------------------------------*
014400970603     C* CARV1 - CARICA VIDEO UNO
014500970603     C*--------------------------------------------------------------------------------------------*
014600970603     C     CARV1         BEGSR
014700970603     C*
014800970603     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014900970603     C*
015000970603     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015100970603     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015200970603     C*
015300970603     C                   ENDSR
015400970603     C*--------------------------------------------------------------------------------------------*
015500970603     C* ENTV1 - GESTIONE TASTO ENTER
015600970603     C*--------------------------------------------------------------------------------------------*
015700970603     C     ENTV1         BEGSR
015800970603     C*
015900970603     C* CONTROLLA I CAMPI IMMESSI
016000970603     C                   EXSR      DECODI
016100970603     C*
016200970603     C                   ENDSR
016300970603     C*--------------------------------------------------------------------------------------------*
016400970603     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
016500970603     C*--------------------------------------------------------------------------------------------*
016600970603     C     F06GES        BEGSR
016700970603     C*
016800970603     C* CONTROLLA I CAMPI IMMESSI
016900160927     C                   EXSR      DECODI
017000970603     C*
017100160927     C* Esegue la richiesta
017200970603IF  1C     $ERR          IFEQ      '0'
017300160927     C                   EXSR      EseguiSQL
017400970603E   1C                   ENDIF
017500130830     C*
017600970603     C                   ENDSR
017700970603     C*--------------------------------------------------------------------------------------------*
017800970603     C* DECODI - CONTROLLA CAMPI E DECODIFICA
017900970603     C*--------------------------------------------------------------------------------------------*
018000970603     C     DECODI        BEGSR
018100970603     C*
018200160927     C* in caso di annullamento non ha senso fare la decodifica: è tutto protetto
018300160927     C                   IF        VMS5Rich = '4'
018400160927     C                   LEAVESR
018500160927     C                   ENDIF
018600160927     C*
018700970603     C* REIMPOSTA INDICATORI DI ERRORE
018800970603     C                   SETOFF                                       606162
018900160715     C                   SETOFF                                       63
019000160715     C*---
019100160715     C* Campi obbligatori
019200160715     C*---
019300160715IF  1C     $err          IFEQ      '0'
019400160927IF  2C                   IF        V1CProc  = *blank
019500160715     C                   MOVEL     '1'           $err
019600160902     C                   MOVEL     MSG(7)        V1MSG
019700160715     C                   SETON                                        60
019800160715     C                   LEAVESR
019900160715E   2C                   ENDIF
020000160715E   1C                   ENDIF
020100160927
020200160715IF  1C     $err          IFEQ      '0'
020300160927IF  2C                   IF        V1CDDE  = 0
020400160715     C                   MOVEL     '1'           $err
020500160902     C                   MOVEL     MSG(7)        V1MSG
020600160902     C                   SETON                                        61
020700160715     C                   LEAVESR
020800160715E   2C                   ENDIF
020900160715E   1C                   ENDIF
021000160927
021100160715IF  1C     $err          IFEQ      '0'
021200160927IF  2C                   IF        V1CDSC  = 0
021300160715     C                   MOVEL     '1'           $err
021400160902     C                   MOVEL     MSG(7)        V1MSG
021500160902     C                   SETON                                        62
021600160715     C                   LEAVESR
021700160715E   2C                   ENDIF
021800160715E   1C                   ENDIF
021900160715     C*---
022000160715     C* DECORRENZA
022100160715     C*---
022200160715IF  1C     $err          IFEQ      '0'
022300160715     C                   MONITOR
022400160715     C* se il valore della data è > 6 cifre
022500160927     C                   IF        %len(%trim(%char(V1CDDE))) > 6
022600160715     C* presumo sia scritta in ggmmaaaa
022700160927IF  5C                   EVAL      DDEISO = %date(V1CDDE:*eur)
022800160715     C* se il valore della data è <= 6 cifre
022900160715     C                   ELSE
023000160715     C* presumo sia scritta in ggmmaa
023100160927IF  5C                   EVAL      DDEISO = %date(V1CDDE:*dmy)
023200160715     C                   ENDIF
023300160715IF  2C                   ON-ERROR
023400160715     C                   MOVEL     '1'           $err
023500160902     C                   SETON                                        61
023600160902     C                   MOVEL     MSG(8)        V1MSG
023700160715     C                   LEAVESR
023800160715     C                   ENDMON
023900160715     C* rivalorizzo il video con la data in ggmmaaaa
024000160927     C                   EVAL      V1CDDE  = %dec(DDEISO:*eur)
024100160715E   1C                   ENDIF
024200160715     C*---
024300160715     C* SCADENZA
024400160715     C*---
024500160715IF  1C     $err          IFEQ      '0'
024600160715     C                   MONITOR
024700160715     C* se il valore della data è > 6 cifre
024800160927     C                   IF        %len(%trim(%char(V1CDSC))) > 6
024900160715     C* presumo sia scritta in ggmmaaaa
025000160927IF  5C                   EVAL      DSCISO = %date(V1CDSC:*eur)
025100160715     C* se il valore della data è <= 6 cifre
025200160715     C                   ELSE
025300160715     C* presumo sia scritta in ggmmaa
025400160927IF  5C                   EVAL      DSCISO = %date(V1CDSC:*dmy)
025500160715     C                   ENDIF
025600160715IF  2C                   ON-ERROR
025700160715     C                   MOVEL     '1'           $err
025800160902     C                   SETON                                        62
025900160902     C                   MOVEL     MSG(8)        V1MSG
026000160715     C                   LEAVESR
026100160715     C                   ENDMON
026200160715     C* rivalorizzo il video con la data in ggmmaaaa
026300160927     C                   EVAL      V1CDSC  = %dec(DSCISO:*eur)
026400160715E   1C                   ENDIF
026500160715     C*---
026600160715     C* la SCADENZA deve essere > DECORRENZA (errore se decorrenza >= scadenza)
026700160715IF  1C     $err          IFEQ      '0'
026800160715IF  2C                   IF        DDEISO >= DSCISO
026900160902     C                   SETON                                        6162
027000160902     C                   MOVEL     MSG(9)        V1MSG
027100160715     C                   MOVEL     '1'           $err
027200160715     C                   LEAVESR
027300160715E   2C                   ENDIF
027400160715E   1C                   ENDIF
027500160715     C* la SCADENZA deve essere >= DATA CORRENTE (errore se scadeza < oggi)
027600170720     C* NON PIÙ VERO
027700160715IF  1C     $err          IFEQ      '0'
027800160715IF  2C                   IF        DSCISO < %date()
027900170720     C***                SETON                                        62
028000170720     C***                MOVEL     MSG(12)       V1MSG
028100170720     C***                MOVEL     '1'           $err
028200170720     C***                LEAVESR
028300160715E   2C                   ENDIF
028400160715E   1C                   ENDIF
028500160927     C*---
028600160927     C* Campi non possono essere contemporaneamente gli stessi d'ingresso
028700160927     C*---
028800160927    1C     $err          IFEQ      '0'
028900160927    2C                   IF        V1CProc  = H1CProcI AND
029000160927     C                             V1CKsu   = H1CKsuI  AND
029100160927     C                             %dec(DDEISO) = H1CDDEI  AND
029200170523     C                             %dec(DSCISO) = H1CDSCI  AND
029300170523     C                             V1CDesProc = H1CDesProc
029400160927     C                   MOVEL     '1'           $err
029500160927     C                   MOVEL     MSG(13)       V1MSG
029600160927     C                   SETON                                        606162
029700160927     C                   SETON                                        63
029800160927     C                   LEAVESR
029900160927E   2C                   ENDIF
030000160927E   1C                   ENDIF
030100160715     C*---
030200160715     C* ACCAVALLAMENTO DATI
030300160916     C* casi di accavallamento
030400160916     C*                   **********
030500160916     C*                +++++       |
030600160916     C*                   ++++     |
030700160916     C*                   |  +++++ |
030800160916     C*                   |     ++++
030900160916     C*                   |      ++++++++
031000160916     C*              +++++++++++++++++++
031100160916     C*                   +++++++++++++++
031200160916     C*            +++++++++++++++++
031300160916     C* casi di NON accavallamento
031400160916     C*                   **********
031500160916     C*          +++++++  |        |
031600160916     C*                   |        |  +++++++
031700160916     C*                   ++++++++++
031800160916     C*
031900160715IF  1C     $err          IFEQ      '0'
032000160927     C                   EVAL      VMSProc = V1CProc
032100160927     C                   EVAL      VMSKSU  = V1CKSU
032200160831     C     K02VMS01      CHAIN(N)  TIVMS01I                           99
032300160831     C* se non esistono rcd con Procedura/Cliente, allora non ci può essere accavallamento
032400160831DO  2C                   DOW       *in99 = *off
032500160831
032600160927     C* questo pgm parte sicuramente da un rcd NON annullato
032700160715
032800160927     C* se non siamo in uno dei rcd che andranno modificati
032900160927IF   C                   IF        VMSProc <> H1CProcI or
033000160927     C                             VMSKsu  <> H1CKsuI  or
033100160927     C                             VMSDDE  <> H1CDDEI  or
033200160927     C                             VMSDSC  <> H1CDSCI
033300160916     C* se non accade quanto sotto, siamo in accavallamento
033400160916     C* se la decorrenza a video è > della scadenza del rcd letto     oppure
033500160916IF  5C                   IF        not(
033600160916IF  5C                             DDEISO > %date(VMSDSC)         OR
033700160916     C*    la scadenza a video è < della decorrenza del rcd letto     oppure
033800160916     C                             DSCISO <= %date(VMSDDE)        OR
033900160916     C*    decorrenza e scadenza a video sono le stesse del rcd letto
034000160916     C                             DDEISO = %date(VMSDDE)     AND
034100160916     C                             DSCISO = %date(VMSDSC)
034200160916     C                             )
034300160715     C                   SETON                                        606162
034400160902     C                   SETON                                        63
034500160902     C                   MOVEL     MSG(11)       V1MSG
034600160715     C                   MOVEL     '1'           $err
034700160715     C                   LEAVE
034800160715E   3C                   ENDIF
034900160927E    C                   ENDIF
035000160715
035100160831     C     K02VMS01      READE(N)  TIVMS01I                               99
035200160715E   2C                   ENDDO
035300160715E   1C                   ENDIF
035400970603     C*
035500970603     C                   ENDSR
035600970603     C*--------------------------------------------------------------------------------------------*
035700970603     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
035800970603     C*--------------------------------------------------------------------------------------------*
035900970603     C     IMPVID        BEGSR
036000160927     C*
036100160927     C                   SETOFF                                       90
036200160927     C                   IF        VMS5Rich = '4'
036300160927     C                   SETON                                        90
036400160927     C                   ENDIF
036500160927     C*
036600160927     C                   EVAL      H1CProcI = VMS5Proc
036700160927     C                   EVAL      H1CKsuI = VMS5Ksu
036800160927     C                   EVAL      H1CDDEI = VMS5DDE
036900160927     C                   EVAL      H1CDSCI = VMS5DSC
037000170523     C                   EVAL      H1CDesProc  = VMS5DesPro
037100160927     C                   CLEAR                   V1CProc
037200160927     C                   CLEAR                   V1CKSU
037300160927     C                   CLEAR                   V1CDDE
037400160927     C                   CLEAR                   V1CDSC
037500170523     C                   CLEAR                   V1CDESPROC
037600160927     C                   EVAL      V1CProc  = VMS5Proc
037700160927     C                   EVAL      V1CKsu  = VMS5Ksu
037800160927     C                   EVAL      V1CDDE  = %dec(%date(VMS5DDE):*eur)
037900160927     C                   EVAL      V1CDSC  = %dec(%date(VMS5DSC):*eur)
038000170523     C                   EVAL      V1CDesProc  = VMS5DesPro
038100970603     C*
038200970603     C                   ENDSR
038300020919     C*--------------------------------------------------------------------------------------------*
038400020919     C* REPERISCE I DATI UTENTE
038500020919     C*--------------------------------------------------------------------------------------------*
038600020919     C     REPDATIUTE    BEGSR
038700020919     C*
038800020919     C* INIZIALIZZA VARIABILI DI WRK
038900020919     C                   CLEAR                   TIBS34DS
039000020919     C                   CLEAR                   AZUTEDS
039100020919     C                   CLEAR                   DDATIUTE
039200020919     C*
039300020919     C     *DTAARA       DEFINE    §azute        azuteds
039400020919     C     *DTAARA       DEFINE    §datiute      ddatiute
039500020919     C                   IN(E)     *DTAARA
039600020919if  1C                   IF        %Error
039700020919     c                   EVAL      I34Tla = 'L'
039800020919     C                   CALL      'TIBS34R'
039900020919     C                   PARM                    Tibs34Ds
040000020919     C                   IN        *DTAARA
040100020919e   1C                   ENDIF
040200020919     C*
040300020919     C                   ENDSR
040400970603     C*--------------------------------------------------------------------------------------------*
040500970603     C* *INZSR  - ROUTINE INIZIALE
040600970603     C*--------------------------------------------------------------------------------------------*
040700970603     C     *INZSR        BEGSR
040800970603     C*--------------------
040900970603     C* RICEVIMENTO PARAMETRI
041000970603     C*--------------------
041100970603     C     *ENTRY        PLIST
041200970612     C                   PARM                    KPJBA
041300160927     C                   PARM                    TIS7VMS5DS
041400970603     C*--------------------
041500970603     C* VARIABILI RIFERITE AL DATA BASE
041600970603     C*--------------------
041700970603     C*--------------------
041800970603     C* VARIABILI NON RIFERITE AL DATA BASE
041900970603     C*--------------------
042000970603     C*--------------------
042100970603     C* CHIAVI DI LETTURA
042200970603     C*--------------------
042300160715     C* LETTURA TIVMS01I - COMPLETA
042400160914     C     K05VMS01      KLIST
042500160715     C                   KFLD                    VMSProc
042600160715     C                   KFLD                    VMSKSU
042700160831     C                   KFLD                    VMSDDE
042800160831     C                   KFLD                    VMSCauTip
042900160831     C                   KFLD                    VMSCauBRT
043000160715     C* LETTURA TIVMS01I - parziale
043100160831     C     K02VMS01      KLIST
043200160715     C                   KFLD                    VMSProc
043300160715     C                   KFLD                    VMSKSU
043400160831     C* LETTURA TIVMS01I - parziale
043500160831     C     K03VMS01      KLIST
043600160831     C                   KFLD                    VMSProc
043700160831     C                   KFLD                    VMSKSU
043800160831     C                   KFLD                    VMSDDE
043900970603     C***
044000970603     C* IMPOSTAZIONI VIDEO
044100970603     C***
044200160927     C*
044300160927     C                   SELECT
044400160927     C                   WHEN      VMS5Rich = '2'
044500160927     C                   EVAL      Viding = '   MODIFICA'
044600160927     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
044700160927     C                   WHEN      VMS5Rich = '4'
044800160927     C                   EVAL      Viding = '    ANNULLA'
044900160927     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
045000160927     C                   OTHER
045100160927     C                   EVAL      Viding = *all'?'
045200160927     C                   ENDSL
045300970603     C*
045400970603     C* ESEGUE OPERAZIONI INIZIALI STANDARD
045500970603     C                   EXSR      INZSTD
045600970603     C*
045700970603     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
045800970603     C                   EXSR      IMPVID
045900970603     C*
046000970603     C* EMETTO PRIMO FORMATO VIDEO
046100970603     C                   EXSR      CARV1
046200970603     C*
046300970603     C                   ENDSR
046400970603     C*--------------------------------------------------------------------------------------------*
046500970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
046600970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
046700970603     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
046800970603     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
046900970603     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
047000970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
047100970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
047200970603     C*--------------------------------------------------------------------------------------------*
047300970603     C* INZSTD - ROUTINE INIZIALE STANDARD
047400970603     C*--------------------------------------------------------------------------------------------*
047500970603     C     INZSTD        BEGSR
047600970603     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
047700970603     C                   CALL      'XKEYS'
047800970603     C                   PARM                    DSKEYS
047900970603     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
048000970603     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
048100970603     C     XMSGPA        PLIST
048200970603     C                   PARM                    DSMSMS
048300970603     C                   PARM                    DSMSPG
048400970603     C                   PARM                    DSMSKE
048500970603     C                   PARM                    DSMSER
048600970603     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
048700160927     C                   MOVEL     'TIS7VMS5R'   DSMSPG
048800160927     C                   MOVEL     'TIS7VMS5R'   VMCQUE
048900970603     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
049000970603     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
049100970603     C* INIZIALIZZO FLAG CONTROLLO PGM
049200970603     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
049300970603     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
049400970603     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
049500970603     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
049600970603     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
049700970603     C                   MOVEL     '0'           $FINE             1            *FINE PGM
049800970603     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
049900970603     C* LANCIO ROUTINE PER VERIFICARE SE
050000970603     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
050100970603     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
050200020919     C                   EXSR      REPDATIUTE
050300970603     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
050400970603     C                   BITOFF    '01234567'    POSMI1            1
050500970603     C                   BITOFF    '01234567'    POSMI2            1
050600970603     C                   BITOFF    '01234567'    POSMA1            1
050700970603     C                   BITOFF    '01234567'    POSMA2            1
050800970603     C                   BITON     '34'          POSMI1            1            *18
050900970603     C                   BITON     '7'           POSMI2            1            *01
051000970603     C                   BITON     '34'          POSMA1            1            *18
051100970603     C                   BITON     '0'           POSMA2            1            *80
051200970603     C                   MOVE      POSMI2        POSMIN            2
051300970603     C                   MOVEL     POSMI1        POSMIN
051400970603     C                   MOVE      POSMA2        POSMAX            2
051500970603     C                   MOVEL     POSMA1        POSMAX
051600970603     C                   ENDSR
051700970603     C*--------------------------------------------------------------------------------------------*
051800970603     C* FUNSTD - LANCIA SUBROUTINE STANDARD
051900970603     C*--------------------------------------------------------------------------------------------*
052000970603     C     FUNSTD        BEGSR
052100970603     C                   SELECT
052200970603     C     KEY           WHENEQ    F3
052300970603     C                   EXSR      F03STD
052400970603     C     KEY           WHENEQ    F12
052500970603     C                   EXSR      F12STD
052600970603     C     KEY           WHENEQ    F24
052700970603     C                   EXSR      F24STD
052800970603     C                   OTHER
052900160902     C                   MOVEL     MSG(2)        V1MSG
053000160902     C***                MOVEL     MSG(2)        $MSG
053100160902     C***                EXSR      CARMSG
053200970603     C                   ENDSL
053300970603     C                   ENDSR
053400970603     C*--------------------------------------------------------------------------------------------*
053500970603     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
053600970603     C*--------------------------------------------------------------------------------------------*
053700970603     C     F03STD        BEGSR
053800970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
053900970603     C                   MOVEL     '1'           $FINE
054000970603     C* IMPOSTA FLAG DI FINE PROCEDURA
054100970603     C                   MOVEL     '1'           $F03
054200970603     C* PULISCE IL MESSAGGIO
054300160927     C                   MOVEL     *BLANKS       VMS5MSG
054400970603     C                   ENDSR
054500970603     C*--------------------------------------------------------------------------------------------*
054600970603     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
054700970603     C*--------------------------------------------------------------------------------------------*
054800970603     C     F12STD        BEGSR
054900970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
055000970603     C                   MOVEL     '1'           $FINE
055100970603     C* IMPOSTA FLAG DI FINE PROCEDURA
055200970603     C                   MOVEL     '1'           $F12
055300970603     C* PULISCE IL MESSAGGIO
055400160927     C                   MOVEL     *BLANKS       VMS5MSG
055500970603     C                   ENDSR
055600970603     C*--------------------------------------------------------------------------------------------*
055700970603     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
055800970603     C*--------------------------------------------------------------------------------------------*
055900970603     C     F24STD        BEGSR
056000970603     C*
056100970603     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
056200970603     C                   EXSR      OUTFUN
056300970603     C*
056400970603     C                   ENDSR
056500970603     C*--------------------------------------------------------------------------------------------*
056600970603     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
056700970603     C*          CODA MESSAGGI DEL PGM
056800970603     C*--------------------------------------------------------------------------------------------*
056900970603     C     CARMSG        BEGSR
057000970603     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
057100970603     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
057200970603     C     MSGULT        IFEQ      0
057300970603     C                   MOVEL     '1'           *IN40
057400160927     C                   WRITE     S7V5DVK
057500970603     C                   MOVEL     '0'           *IN40
057600970603     C                   ENDIF
057700970603     C* INVIA MESSAGGIO ALLA CODA DEL PGM
057800970603     C                   MOVEL     $MSG          DSMSMS
057900970603     C                   CALL      'XMSG'        XMSGPA
058000970603     C     DSMSER        IFEQ      0
058100970603     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
058200970603     C                   ENDIF
058300970603     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
058400970603     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
058500160927     C                   WRITE     S7V5DVM                                      *SFL MESS
058600970603     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
058700970603     C                   MOVEL     *BLANKS       $MSG
058800970603     C                   ENDSR
058900970603     C*--------------------------------------------------------------------------------------------*
059000970603     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
059100970603     C*--------------------------------------------------------------------------------------------*
059200970603     C     OUTMSG        BEGSR
059300970603     C* VISUALIZZAZIONE MESSAGGIO
059400970603     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
059500160927     C                   WRITE     S7V5DVK                                      *CONTR MESS
059600970603     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
059700970603     C                   Z-ADD     *ZEROS        MSGULT
059800970603     C                   ENDSR
059900970603     C*--------------------------------------------------------------------------------------------*
060000970603     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
060100970603     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
060200970603     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
060300970603     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
060400970603     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
060500970603     C*--------------------------------------------------------------------------------------------*
060600970603     C     CARFUN        BEGSR
060700970603     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
060800970603     C     1             DO        31            I                 5 0          --- 1 -->
060900970603     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
061000970603     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
061100970603     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
061200020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
061300970603     C     $TGES         IFEQ      'G'                                          *GESTIONE
061400970603     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
061500970603     C                   ELSE                                                   *INTERROG.
061600970603     C                   MOVEL     FUNASI        FAT(I)
061700970603     C                   ENDIF
061800970603     C                   ELSE                                                   <-- 2 --> *FILIALE
061900970603     C     $TGES         IFEQ      'G'                                          *GESTIONE
062000970603     C                   MOVEL     FUNAFG        FAT(I)
062100970603     C                   ELSE                                                   *INTERROG.
062200970603     C                   MOVEL     FUNAFI        FAT(I)
062300970603     C                   ENDIF
062400970603     C                   ENDIF                                                  <-- 2 ---
062500970603     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
062600970603     C                   MOVEL     USER          WEDP              3            *PROFILO
062700970603     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
062800970603     C                   MOVEL     'S'           FAT(I)                         *ABILITO
062900970603     C                   ELSE
063000970603     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
063100970603     C                   ENDIF
063200970603     C                   ENDIF
063300970603     C                   ENDDO                                                  <-- 1 ---
063400970603     C                   ENDSR
063500970603     C*--------------------------------------------------------------------------------------------*
063600970603     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
063700970603     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
063800970603     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
063900970603     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
064000970603     C*--------------------------------------------------------------------------------------------*
064100970603     C     OUTFUN        BEGSR
064200970603     C     1             DO        8             I                              *PULIZIA FUNZIONI A
064300970603     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
064400970603     C                   ENDDO
064500970603     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
064600970603     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
064700970603     C     1             DO        31            I
064800970603     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
064900970603     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
065000970603     C                   ADD       1             FUNNAT
065100970603     C                   ENDIF
065200970603     C                   ENDDO
065300970603     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
065400970603     C     FUNNAT        IFGT      8
065500970603     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
065600970603     C                   ENDIF
065700970603     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
065800970603     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
065900970603     C                   Z-ADD     8             FUNVIS            5 0
066000970603     C     FAT(23)       IFEQ      'S'
066100970603     C                   SUB       1             FUNVIS
066200970603     C                   ENDIF
066300970603     C     FAT(24)       IFEQ      'S'
066400970603     C                   SUB       1             FUNVIS
066500970603     C                   ENDIF
066600970603     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
066700970603     C                   Z-ADD     *ZEROS        W                 5 0
066800970603     C                   ADD       1             FUNULT
066900970603     C     FUNULT        IFGT      31
067000970603     C                   Z-ADD     1             FUNULT
067100970603     C                   ENDIF
067200970603     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
067300970603     C     FUNULT        DO        31            I                              --- 1 -->
067400970603     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
067500970603     C     W             IFLT      FUNVIS                                       --- 2 -->
067600970603     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
067700970603     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
067800970603     C     FDE(I)        ANDNE     *BLANKS
067900970603     C     I             IFEQ      23                                           *TOLGO F23
068000970603     C     I             OREQ      24                                           *E F24
068100970603     C                   ELSE
068200970603     C                   ADD       1             W
068300970603     C                   MOVEL     FDE(I)        FUV(W)
068400970603     C                   ENDIF
068500970603     C                   ENDIF                                                  <-- 3 ---
068600970603     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
068700970603     C                   Z-ADD     I             FUNULT
068800970603     C                   ENDIF                                                  <-- 2 ---
068900970603     C                   ENDDO                                                  <-- 1 ---
069000970603     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
069100970603     C                   MOVEL     FUV(1)        VFD001
069200970603     C                   MOVEL     FUV(2)        VFD002
069300970603     C                   MOVEL     FUV(3)        VFD003
069400970603     C                   MOVEL     FUV(4)        VFD004
069500970603     C                   MOVEL     FUV(5)        VFD005
069600970603     C                   MOVEL     FUV(6)        VFD006
069700970603     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
069800970603     C     FAT(24)       ANDEQ     'N'
069900970603     C                   MOVEL     FUV(7)        VFD007
070000970603     C                   MOVEL     FUV(8)        VFD008
070100970603     C                   ELSE                                                   <-- 1 -->
070200970603     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
070300970603     C     FAT(24)       ANDEQ     'S'
070400970603     C                   MOVEL     FDE(23)       VFD007
070500970603     C                   MOVEL     FDE(24)       VFD008
070600970603     C                   ELSE                                                   <-- 2 -->
070700970603     C                   MOVEL     FUV(7)        VFD007
070800970603     C     FAT(23)       IFEQ      'S'
070900970603     C                   MOVEL     FDE(23)       VFD008
071000970603     C                   ELSE
071100970603     C                   MOVEL     FDE(24)       VFD008
071200970603     C                   ENDIF
071300970603     C                   ENDIF                                                  <-- 2 ---
071400970603     C                   ENDIF                                                  <-- 1 ---
071500970603     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
071600970603     C                   ENDSR
071700970603     C*--------------------------------------------------------------------------------------------*
071800970603     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
071900970603     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
072000970603     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
072100970603     C*                   - ERRORE SE FUNZIONE NON ATTIVA
072200970603     C*--------------------------------------------------------------------------------------------*
072300970603     C     CONFUN        BEGSR
072400970603     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
072500970603     C     1             DO        31            I                              --- 1 -->
072600970603     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
072700970603     C* CONTROLLO SE IL TASTO E' ATTIVO
072800970603     C     FAT(I)        IFNE      'S'                                          --- 3 -->
072900160902     C                   MOVEL     MSG(1)        V1MSG                          *FUNZ.NON ACCETTABIL
073000160902     C***                MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
073100160902     C***                EXSR      CARMSG
073200970603     C                   MOVEL     '1'           $ERR
073300970603     C                   ENDIF                                                  <-- 3 ---
073400970603     C                   ENDIF                                                  <-- 2 ---
073500970603     C                   ENDDO                                                  <-- 1 ---
073600970603     C                   ENDSR
073700160927
073800160927      /free
073900160927
074000160927       //--------------------------------------------------------------
074100160927       // Esecuzione aggiornamento di massa
074200160927       //--------------------------------------------------------------
074300160927       BEGSR  eseguiSQL;
074400160927
074500160927         // preparo SQL a seconda della richiesta
074600160927         select;
074700160927          when VMS5Rich = '2';
074800160927           wSQL = 'update tivms00f +
074900160927                 set +
075000160927                 vmsproc = ''' + V1CProc + ''', +
075100160927                 vmsksu  = ''' + V1CKsu  + ''', +
075200160927                 vmsdde  = ' + %char(%dec(DDEISO)) + ', +
075300170523                 vmsdsc  = ' + %char(%dec(DSCISO)) + ', +
075400170523                 vmsdesproc  = ''' + V1CDesProc + ''' +
075500170523                 where +
075600160927                 vmsproc = ''' + H1CProcI + ''' and +
075700160927                 vmsksu  = ''' + H1CKsuI + ''' and +
075800160927                 vmsdde  = ' + %char(H1CDDEI) + ' and +
075900160927                 vmsdsc  = ' + %char(H1CDSCI) ;
076000160927          when VMS5Rich = '4';
076100160927           wSQL = 'update tivms00f +
076200160927                 set +
076300160927                 vmsatb  = ''A'' +
076400160927                 where +
076500160927                 vmsproc = ''' + H1CProcI + ''' and +
076600160927                 vmsksu  = ''' + H1CKsuI + ''' and +
076700160927                 vmsdde  = ' + %char(H1CDDEI) + ' and +
076800160927                 vmsdsc  = ' + %char(H1CDSCI) ;
076900160927         endsl;
077000160927
077100160927         // eseguo
077200160927         exec sql execute immediate :wSQL;
077300160927
077400160927         // errore SQL
077500160927         if sqlCode < 0;
077600160927           $err = '1';
077700160927           V1Msg = MSG(19);
077800160927         else;
077900160927           V1Msg = MSG(20);
078000160927         endif;
078100160927
078200160927       ENDSR;
078300160927
078400160927      /end-free
078500160927
078600970603     O*--------------------------------------------------------------------------------------------*
078700970603** -MSG- (MESSAGGI)
078800160927TIS7VMS5R- Tasto funzionale non valido.                                        1
078900160927TIS7VMS5R- Tasto funzionale abilitato ma non gestito. Avvertire CED.           2
079000160927TIS7VMS5R- Opzione non valida.                                                 3
079100160927TIS7VMS5R- Opzione abilitata ma non gestita. Avvertire CED.                    4
079200160927TIS7VMS5R- Inizio elenco.                                                      5
079300160927TIS7VMS5R- Fine elenco.                                                        6
079400160927TIS7VMS5R- Campo obbligatorio                                                  7
079500160927TIS7VMS5R- Data errata                                                         8
079600160927TIS7VMS5R- La data scadenza deve essere maggiore della decorrenza              9
079700160927TIS7VMS5R- Versione già inserita                                              10
079800160927TIS7VMS5R- La versione non può sovrapporsi ad una già in decorrenza           11
079900160927TIS7VMS5R- La versione non può scadere in un giorno già trascorso             12
080000160927TIS7VMS5R- I valori dei campi non possono essere quelli d'ingresso            13
080100160927TIS7VMS5R- 14
080200160927TIS7VMS5R- 15
080300160927TIS7VMS5R- 16
080400160927TIS7VMS5R- 17
080500160927TIS7VMS5R- 18
080600160927TIS7VMS5R- Errore SQL - Elaborazione NON effettuata                           19
080700160927TIS7VMS5R- Elaborazione effettuata                                            20
080800970603** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
080900970603F1    -                   -    NN     NN
081000970603F2    -                   -    NN     NN
081100970603F3    -F3=Fine            -    NN     NN
081200970603F4    -                   -    NN     NN
081300970603F5    -                   -    NN     NN
081400970603F6    -F6=Conferma        -    SN     SN
081500970603F7    -                   -    NN     NN
081600970603F8    -                   -    NN     NN
081700970603F9    -                   -    NN     NN
081800970603F10   -                   -    NN     NN
081900970603F11   -                   -    NN     NN
082000970603F12   -F12=Ritorno        -    SS     SS
082100970603F13   -                   -    NN     NN
082200970603F14   -                   -    NN     NN
082300970603F15   -                   -    NN     NN
082400970603F16   -                   -    NN     NN
082500970603F17   -                   -    NN     NN
082600970603F18   -                   -    NN     NN
082700970603F19   -                   -    NN     NN
082800970603F20   -                   -    NN     NN
082900970603F21   -                   -    NN     NN
083000970603F22   -                   -    NN     NN
083100970603F23   -                   -    NN     NN
083200970603F24   -F24=Altri tasti    -    NN     NN
083300970603ENTER -                   -    SS     SS
083400970603ROLLUP-                   -    NN     NN
083500970603ROLLDN-                   -    NN     NN
083600970603HELP  -                   -    NN     NN
083700970603PRINT -                   -    NN     NN
083800970603BSPACE-                   -    NN     NN
083900970603CLEAR -                   -    NN     NN
