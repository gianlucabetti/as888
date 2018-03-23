000100970603     H*--------------------------------------------------------------------------------------------*
000200970603     H DECEDIT('0,') DATEDIT(*DMY.)
000300970603     F*--------------------------------------------------------------------------------------------*
000400970603     F* DATA BASE
000500970603     F*--------------------------------------------------------------------------------------------*
000600160715     FTIS7VMS3D CF   E             WORKSTN INFDS(DEVDS1)
000700160715     F                                     SFILE(S7V3DVM:MSGULT)
000800160715     FTIVMS01I  IF   E           K DISK    rename(TIVMS000:TIVMS01)
000900160715     FTIVMS00F  IF A E           K DISK    prefix(F_)
001000970603     D*--------------------------------------------------------------------------------------------*
001100970603     D* SCHIERE
001200970603     D*--------------------------------------------------------------------------------------------*
001300970603     D*-------------------
001400970603     D* MESSAGGI E FUNZIONI
001500970603     D*-------------------
001600970603     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001700970603     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001800970603     D*-------------------
001900970603     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002000970603     D*-------------------
002100970603     D FFU             S              6    DIM(31)                              *FUNZIONE
002200970603     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002300970603     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002400970603     D*-------------------
002500970603     D* TABELLA DESCRIZIONI A VIDEO
002600970603     D*-------------------
002700970603     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002800970603     D*-------------------
002900970603     D* TABELLA TASTI FUNZIONALI PREMUTI
003000970603     D*-------------------
003100970603     D KEYS            S              1    DIM(40)
003200970603     D*--------------------------------------------------------------------------------------------*
003300970603     D* INPUT
003400970603     D*--------------------------------------------------------------------------------------------*
003500970603     D*-------------------
003600970603     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003700970603     D*-------------------
003800970603     D DSFUN         E DS
003900970603     D  FUNRIG                 1     40
004000970603     D*-------------------
004100970603     D* DS PER GESTIONE TASTI FUNZIONALI
004200970603     D*-------------------
004300970603     D DSKEYS        E DS
004400970603     D  KEYRIG                 1     40
004500970603     D*-------------------
004600970603     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004700970603     D*-------------------
004800970603     D                SDS
004900970603     D  USER                 254    263
005000970603     D*-------------------
005100970603     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005200970603     D*-------------------
005300970603     D DEVDS1          DS
005400970603     D  KEY                  369    369
005500970603     D  CURSOR               370    371
005600970603     D*---------------------
005700970603     D* ARCHITETTURA
005800970603     D*---------------------
005900970603     D KPJBA         E DS
006000970603     D*---------------------
006100970603     D* DS ESTERNA DI PROCEDURA
006200970603     D*---------------------
006300160715     D TIS7VMS2DS    E DS
006400970603     D*-------------------
006500020919     D* DS REPERIMENTO DATI UTENTE
006600970603     D*-------------------
006700020919     D TIBS34DS      E DS                                                       *Profili utente
006800020919     D DDATIUTE      E DS                                                       *Dati utente
006900020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007000970603     D*-------------------
007100970603     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007200970603     D*-------------------
007300970603     D DSMSG         E DS
007400970603     D*------------------
007500970603     D* DS "XSRDA8" - CONTROLLA DATA (8)
007600970603     D*------------------
007700970603     D WLBDA8          DS
007800970603     D  G08DAT                 1      8  0
007900970603     D  G08INV                 9     16  0
008000970603     D  G08ERR                17     17
008100970603     D  G08TGI                18     22  0
008200160715     D*------------
008300160715     D* campi tipo data per le date a video
008400160715     D DDEISO          s               d
008500160715     D DSCISO          s               d
008600970603     C*--------------------------------------------------------------------------------------------*
008700970603     C* MAIN LINE
008800970603     C*--------------------------------------------------------------------------------------------*
008900970603     C*
009000970611     C* SE NON CI SONO ERRORI IN ENTRATA
009100970611IF  1C     $FINE         IFEQ      '0'
009200160715     C                   WRITE     S7V3DVT                                      *TESTATA
009300970611DO  2C     $FINE         DOUEQ     '1'
009400970603     C*
009500970603     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009600970611IF  3C     $GES          IFEQ      'V1'
009700970603     C                   EXSR      GESV1
009800970611E   3C                   ENDIF
009900970603     C*
010000970611E   2C                   ENDDO
010100970603     C*
010200970611IF  2C     $F03          IFEQ      '1'
010300160715     C                   MOVEL     '1'           VMS2F03
010400970611E   2C                   ENDIF
010500970611E   1C                   ENDIF
010600970603     C* FINE PGM
010700970603     C                   SETON                                        LR
010800970603     C*--------------------------------------------------------------------------------------------*
010900970603     C* GESV1 - GESTIONE VIDEO UNO
011000970603     C*--------------------------------------------------------------------------------------------*
011100970603     C     GESV1         BEGSR
011200970603     C*
011300970603IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
011400970603     C                   EXSR      CARV1                                        *CARICA VIDEO
011500970603E   1C                   ENDIF
011600970603     C*
011700160902     C* QUESTA TECNICA NON FUNZIONA NELLE FINESTRE!
011800160902IF  1C*    MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011900160902     C*                  MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012000160902     C*                  EXSR      CARMSG                                       *CARICA MESSAGGIO
012100160902E   1C*                  ENDIF
012200160902     C*                  EXSR      OUTMSG                                       *EMETTE MESSAGGI
012300970603     C*
012400160715     C                   EXFMT     S7V3DV1                                      *PARAMETRI
012500160902     C                   MOVEL     *blank        V1MSG                          pulisco errmsg
012600970603     C*
012700970603     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012800970603     C*
012900970603IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013000970603SL  2C                   SELECT
013100970603     C     KEY           WHENEQ    ENTER
013200970603     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013300970603     C     KEY           WHENEQ    F6
013400970603     C                   EXSR      F06GES
013500970603     C                   OTHER
013600970603     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013700970603E   2C                   ENDSL
013800970603E   1C                   ENDIF
013900970603     C*
014000970603     C                   ENDSR
014100970603     C*--------------------------------------------------------------------------------------------*
014200970603     C* CARV1 - CARICA VIDEO UNO
014300970603     C*--------------------------------------------------------------------------------------------*
014400970603     C     CARV1         BEGSR
014500970603     C*
014600970603     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014700970603     C*
014800970603     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014900970603     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015000970603     C*
015100970603     C                   ENDSR
015200970603     C*--------------------------------------------------------------------------------------------*
015300970603     C* ENTV1 - GESTIONE TASTO ENTER
015400970603     C*--------------------------------------------------------------------------------------------*
015500970603     C     ENTV1         BEGSR
015600970603     C*
015700970603     C* CONTROLLA I CAMPI IMMESSI
015800970603     C                   EXSR      DECODI
015900970603     C*
016000970603     C                   ENDSR
016100970603     C*--------------------------------------------------------------------------------------------*
016200970603     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
016300970603     C*--------------------------------------------------------------------------------------------*
016400970603     C     F06GES        BEGSR
016500970603     C*
016600970603     C* CONTROLLA I CAMPI IMMESSI
016700970603     C                   EXSR      DECODI
016800970603     C*
016900160831     C* LEGGE LA Procedura/KSU/Decorrenza DA CUI COPIARE
017000970603IF  1C     $ERR          IFEQ      '0'
017100160715     C                   EVAL      VMSProc = V1CProcD
017200160715     C                   EVAL      VMSKSU  = V1CKSUD
017300160913     C                   EVAL      VMSDDE    = %dec(VMS2DDE:8:0)
017400160831     C     K03VMS01      CHAIN(N)  TIVMS01                            99
017500160715DO  2C     *IN99         DOWEQ     *OFF
017600160715     C                   EVAL      F_VMSProc = V1CProcA
017700160715     C                   EVAL      F_VMSKSU  = V1CKSUA
017800160715     C                   EVAL      F_VMSDDE  = %dec(DDEISO)
017900160715     C                   EVAL      F_VMSDSC  = %dec(DSCISO)
018000160715     C                   EXSR      ImpVMS
018100160715     C                   WRITE     TIVMS000
018200170221     C     K03VMS01      READE(N)  TIVMS01                                99
018300160715E   2C                   ENDDO
018400970603     C*
018500970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
018600160715     C                   MOVEL     MSG(20)       VMS2MSG
018700970603     C                   MOVEL     '1'           $FINE
018800970603E   1C                   ENDIF
018900130830     C*
019000970603     C                   ENDSR
019100160715     C*------------------------------------------------------------------------*
019200160715     C* IMPVMS - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
019300160715     C*------------------------------------------------------------------------*
019400160715     C     IMPVMS        BEGSR
019500160715     C*
019600160831     C                   EVAL      F_VMSCAUTIP = VMSCAUTIP
019700160831     C                   EVAL      F_VMSCAUBRT = VMSCAUBRT
019800160715     C                   EVAL      F_VMSSTSCLI = VMSSTSCLI
019900160715     C                   EVAL      F_VMSCAUTT  = VMSCAUTT
020000160715     C                   EVAL      F_VMSSTSOPZ = VMSSTSOPZ
020100160715     C                   EVAL      F_VMSFASOP  = VMSFASOP
020200160715     C                   EVAL      F_VMSFLO    = VMSFLO
020300170221     C                   EVAL      F_VMSDESCRIZ= VMSDESCRIZ
020400170221     C                   EVAL      F_VMSDESCIT = VMSDESCIT
020500160715     C                   EVAL      F_VMSDESCEN = VMSDESCEN
020600160715     C                   EVAL      F_VMSDESCFR = VMSDESCFR
020700160715     C                   EVAL      F_VMSDESCDE = VMSDESCDE
020800160715     C                   EVAL      F_VMSDESCES = VMSDESCES
020900170523     C                   EVAL      F_VMSDESPROC= VMSDESPROC
021000160831     C                   EVAL      F_VMSATB    = *blank
021100160715     C*
021200160715     C                   ENDSR
021300970603     C*--------------------------------------------------------------------------------------------*
021400970603     C* DECODI - CONTROLLA CAMPI E DECODIFICA
021500970603     C*--------------------------------------------------------------------------------------------*
021600970603     C     DECODI        BEGSR
021700970603     C*
021800970603     C* REIMPOSTA INDICATORI DI ERRORE
021900970603     C                   SETOFF                                       606162
022000160715     C                   SETOFF                                       63
022100160715     C*---
022200160715     C* Campi obbligatori
022300160715     C*---
022400160715IF  1C     $err          IFEQ      '0'
022500160715IF  2C                   IF        V1CProcA = *blank
022600160715     C                   MOVEL     '1'           $err
022700160902     C                   MOVEL     MSG(7)        V1MSG
022800160902     C***                MOVEL     MSG(7)        $MSG
022900160902     C***                EXSR      CARMSG
023000160715     C                   SETON                                        60
023100160715     C                   LEAVESR
023200160715E   2C                   ENDIF
023300160715E   1C                   ENDIF
023400160715IF  1C     $err          IFEQ      '0'
023500160902IF  2C***                IF        V1CKSUA = *blank
023600160902     C***                MOVEL     '1'           $err
023700160902     C***                MOVEL     MSG(7)        $MSG
023800160902     C***                EXSR      CARMSG
023900160902     C***                SETON                                        61
024000160902     C***                LEAVESR
024100160902E   2C***                ENDIF
024200160715E   1C                   ENDIF
024300160715IF  1C     $err          IFEQ      '0'
024400160715IF  2C                   IF        V1CDDEA = 0
024500160715     C                   MOVEL     '1'           $err
024600160902     C                   MOVEL     MSG(7)        V1MSG
024700160902     C***                MOVEL     MSG(7)        $MSG
024800160902     C***                EXSR      CARMSG
024900160902     C                   SETON                                        61
025000160715     C                   LEAVESR
025100160715E   2C                   ENDIF
025200160715E   1C                   ENDIF
025300160715IF  1C     $err          IFEQ      '0'
025400160715IF  2C                   IF        V1CDSCA = 0
025500160715     C                   MOVEL     '1'           $err
025600160902     C                   MOVEL     MSG(7)        V1MSG
025700160902     C***                MOVEL     MSG(7)        $MSG
025800160902     C***                EXSR      CARMSG
025900160902     C                   SETON                                        62
026000160715     C                   LEAVESR
026100160715E   2C                   ENDIF
026200160715E   1C                   ENDIF
026300160715     C*---
026400160715     C* DECORRENZA
026500160715     C*---
026600160715IF  1C     $err          IFEQ      '0'
026700160715     C                   MONITOR
026800160715     C* se il valore della data è > 6 cifre
026900160715     C                   IF        %len(%trim(%char(V1CDDEA))) > 6
027000160715     C* presumo sia scritta in ggmmaaaa
027100160715IF  5C                   EVAL      DDEISO = %date(V1CDDEA:*eur)
027200160715     C* se il valore della data è <= 6 cifre
027300160715     C                   ELSE
027400160715     C* presumo sia scritta in ggmmaa
027500160715IF  5C                   EVAL      DDEISO = %date(V1CDDEA:*dmy)
027600160715     C                   ENDIF
027700160715IF  2C                   ON-ERROR
027800160715     C                   MOVEL     '1'           $err
027900160902     C                   SETON                                        61
028000160902     C                   MOVEL     MSG(8)        V1MSG
028100160902     C***                MOVEL     MSG(8)        $MSG
028200160902     C***                EXSR      CARMSG
028300160715     C                   LEAVESR
028400160715     C                   ENDMON
028500160715     C* rivalorizzo il video con la data in ggmmaaaa
028600160913     C                   EVAL      V1CDDEA = %dec(DDEISO:*eur)
028700160715E   1C                   ENDIF
028800160715     C*---
028900160715     C* SCADENZA
029000160715     C*---
029100160715IF  1C     $err          IFEQ      '0'
029200160715     C                   MONITOR
029300160715     C* se il valore della data è > 6 cifre
029400160715     C                   IF        %len(%trim(%char(V1CDSCA))) > 6
029500160715     C* presumo sia scritta in ggmmaaaa
029600160715IF  5C                   EVAL      DSCISO = %date(V1CDSCA:*eur)
029700160715     C* se il valore della data è <= 6 cifre
029800160715     C                   ELSE
029900160715     C* presumo sia scritta in ggmmaa
030000160715IF  5C                   EVAL      DSCISO = %date(V1CDSCA:*dmy)
030100160715     C                   ENDIF
030200160715IF  2C                   ON-ERROR
030300160715     C                   MOVEL     '1'           $err
030400160902     C                   SETON                                        62
030500160902     C                   MOVEL     MSG(8)        V1MSG
030600160902     C***                MOVEL     MSG(8)        $MSG
030700160902     C***                EXSR      CARMSG
030800160715     C                   LEAVESR
030900160715     C                   ENDMON
031000160715     C* rivalorizzo il video con la data in ggmmaaaa
031100160715     C                   EVAL      V1CDSCA = %dec(DSCISO:*eur)
031200160715E   1C                   ENDIF
031300160715     C*---
031400160715     C* la SCADENZA deve essere > DECORRENZA (errore se decorrenza >= scadenza)
031500160715IF  1C     $err          IFEQ      '0'
031600160715IF  2C                   IF        DDEISO >= DSCISO
031700160902     C                   SETON                                        6162
031800160902     C                   MOVEL     MSG(9)        V1MSG
031900160902     C***                MOVEL     MSG(9)        $MSG
032000160902     C***                EXSR      CARMSG
032100160715     C                   MOVEL     '1'           $err
032200160715     C                   LEAVESR
032300160715E   2C                   ENDIF
032400160715E   1C                   ENDIF
032500160715     C* la SCADENZA deve essere >= DATA CORRENTE (errore se scadeza < oggi)
032600160715IF  1C     $err          IFEQ      '0'
032700160715IF  2C                   IF        DSCISO < %date()
032800160902     C                   SETON                                        62
032900160902     C                   MOVEL     MSG(12)       V1MSG
033000160902     C***                MOVEL     MSG(12)       $MSG
033100160902     C***                EXSR      CARMSG
033200160715     C                   MOVEL     '1'           $err
033300160715     C                   LEAVESR
033400160715E   2C                   ENDIF
033500160715E   1C                   ENDIF
033600160715     C*---
033700160715     C* INSERIMENTO -> RECORD INESISTENTE
033800160715IF  1C     $err          IFEQ      '0'
033900160715     C                   EVAL      VMSPROC   = V1CPROCA
034000160715     C                   EVAL      VMSKSU    = V1CKSUA
034100160831     C                   EVAL      VMSDDE    = %dec(DDEISO)
034200160914     C* CAUTIP e CAUBRT sono quelli del rcd da copiare, quindi già valorizzati
034300160831     C*                  EVAL      VMSCAUTIP =
034400160831     C*                  EVAL      VMSCAUBRT =
034500160914     C     K05VMS01      CHAIN(N)  TIVMS01                            99
034600160715IF  2C     *IN99         IFEQ      *OFF
034700160715     C                   MOVEL     '1'           $err
034800160902     C                   MOVEL     MSG(10)       V1MSG
034900160902     C***                MOVEL     MSG(10)       $MSG
035000160902     C***                EXSR      CARMSG
035100160715     C                   SETON                                        606162
035200160902     C                   SETON                                        63
035300160715     C                   LEAVESR
035400160715E   2C                   ENDIF
035500160715E   1C                   ENDIF
035600160715     C*---
035700160715     C* ACCAVALLAMENTO DATI
035800160916     C* casi di accavallamento
035900160916     C*                   **********
036000160916     C*                +++++       |
036100160916     C*                   ++++     |
036200160916     C*                   |  +++++ |
036300160916     C*                   |     ++++
036400160916     C*                   |      ++++++++
036500160916     C*              +++++++++++++++++++
036600160916     C*                   +++++++++++++++
036700160916     C*            +++++++++++++++++
036800160916     C* casi di NON accavallamento
036900160916     C*                   **********
037000160916     C*          +++++++  |        |
037100160916     C*                   |        |  +++++++
037200160916     C*                   ++++++++++
037300160916     C*
037400160715IF  1C     $err          IFEQ      '0'
037500160831     C     K02VMS01      CHAIN(N)  TIVMS01I                           99
037600160831     C* se non esistono rcd con Procedura/Cliente, allora non ci può essere accavallamento
037700160831DO  2C                   DOW       *in99 = *off
037800160831
037900160831     C* la duplica parte sicuramente da un rcd NON annullato
038000160715
038100160916     C* se non accade quanto sotto, siamo in accavallamento
038200160916     C* se la decorrenza a video è > della scadenza del rcd letto     oppure
038300160916IF  5C                   IF        not(
038400160916IF  5C                             DDEISO > %date(VMSDSC)         OR
038500160916     C*    la scadenza a video è < della decorrenza del rcd letto     oppure
038600160916     C                             DSCISO <= %date(VMSDDE)        OR
038700160916     C*    decorrenza e scadenza a video sono le stesse del rcd letto
038800160916     C                             DDEISO = %date(VMSDDE)     AND
038900160916     C                             DSCISO = %date(VMSDSC)
039000160916     C                             )
039100160715     C                   SETON                                        606162
039200160902     C                   SETON                                        63
039300160902     C                   MOVEL     MSG(11)       V1MSG
039400160902     C***                MOVEL     MSG(11)       $MSG
039500160902     C***                EXSR      CARMSG
039600160715     C                   MOVEL     '1'           $err
039700160715     C                   LEAVE
039800160715E   3C                   ENDIF
039900160715
040000160831     C     K02VMS01      READE(N)  TIVMS01I                               99
040100160715E   2C                   ENDDO
040200160715E   1C                   ENDIF
040300970603     C*
040400970603     C                   ENDSR
040500970603     C*--------------------------------------------------------------------------------------------*
040600970603     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
040700970603     C*--------------------------------------------------------------------------------------------*
040800970603     C     IMPVID        BEGSR
040900970603     C*
041000160715     C* DATI SU CUI COPIARE
041100160715     C                   CLEAR                   V1CProcA
041200160715     C                   CLEAR                   V1CKSUA
041300160715     C                   CLEAR                   V1CDDEA
041400160715     C                   CLEAR                   V1CDSCA
041500160715     C* DATI DA CUI COPIARE
041600160901     C                   EVAL      V1CProcD =    VMS2Proc
041700160901     C                   EVAL      V1CKSUD  =    VMS2KSU
041800160913     C                   EVAL      V1CDDED  =
041900160913     c                              %dec(%date(%dec(VMS2DDE:8:0)):*eur)
042000970603     C*
042100970603     C                   ENDSR
042200020919     C*--------------------------------------------------------------------------------------------*
042300020919     C* REPERISCE I DATI UTENTE
042400020919     C*--------------------------------------------------------------------------------------------*
042500020919     C     REPDATIUTE    BEGSR
042600020919     C*
042700020919     C* INIZIALIZZA VARIABILI DI WRK
042800020919     C                   CLEAR                   TIBS34DS
042900020919     C                   CLEAR                   AZUTEDS
043000020919     C                   CLEAR                   DDATIUTE
043100020919     C*
043200020919     C     *DTAARA       DEFINE    §azute        azuteds
043300020919     C     *DTAARA       DEFINE    §datiute      ddatiute
043400020919     C                   IN(E)     *DTAARA
043500020919if  1C                   IF        %Error
043600020919     c                   EVAL      I34Tla = 'L'
043700020919     C                   CALL      'TIBS34R'
043800020919     C                   PARM                    Tibs34Ds
043900020919     C                   IN        *DTAARA
044000020919e   1C                   ENDIF
044100020919     C*
044200020919     C                   ENDSR
044300970603     C*--------------------------------------------------------------------------------------------*
044400970603     C* *INZSR  - ROUTINE INIZIALE
044500970603     C*--------------------------------------------------------------------------------------------*
044600970603     C     *INZSR        BEGSR
044700970603     C*--------------------
044800970603     C* RICEVIMENTO PARAMETRI
044900970603     C*--------------------
045000970603     C     *ENTRY        PLIST
045100970612     C                   PARM                    KPJBA
045200160715     C                   PARM                    TIS7VMS2DS
045300970603     C*--------------------
045400970603     C* VARIABILI RIFERITE AL DATA BASE
045500970603     C*--------------------
045600970603     C*--------------------
045700970603     C* VARIABILI NON RIFERITE AL DATA BASE
045800970603     C*--------------------
045900970603     C*--------------------
046000970603     C* CHIAVI DI LETTURA
046100970603     C*--------------------
046200160715     C* LETTURA TIVMS01I - COMPLETA
046300160914     C     K05VMS01      KLIST
046400160715     C                   KFLD                    VMSProc
046500160715     C                   KFLD                    VMSKSU
046600160831     C                   KFLD                    VMSDDE
046700160831     C                   KFLD                    VMSCauTip
046800160831     C                   KFLD                    VMSCauBRT
046900160715     C* LETTURA TIVMS01I - parziale
047000160831     C     K02VMS01      KLIST
047100160715     C                   KFLD                    VMSProc
047200160715     C                   KFLD                    VMSKSU
047300160831     C* LETTURA TIVMS01I - parziale
047400160831     C     K03VMS01      KLIST
047500160831     C                   KFLD                    VMSProc
047600160831     C                   KFLD                    VMSKSU
047700160831     C                   KFLD                    VMSDDE
047800970603     C***
047900970603     C* IMPOSTAZIONI VIDEO
048000970603     C***
048100160715IF  1C     VMS2OP0       IFEQ      'R02'
048200970603     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
048300970603X   1C                   ELSE
048400970603     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
048500970603E   1C                   ENDIF
048600970603     C*
048700970603     C* ESEGUE OPERAZIONI INIZIALI STANDARD
048800970603     C                   EXSR      INZSTD
048900970603     C*
049000970603     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
049100970603     C                   EXSR      IMPVID
049200970603     C*
049300970603     C* EMETTO PRIMO FORMATO VIDEO
049400970603     C                   EXSR      CARV1
049500970603     C*
049600970603     C                   ENDSR
049700970603     C*--------------------------------------------------------------------------------------------*
049800970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049900970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
050000970603     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
050100970603     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
050200970603     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
050300970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
050400970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
050500970603     C*--------------------------------------------------------------------------------------------*
050600970603     C* INZSTD - ROUTINE INIZIALE STANDARD
050700970603     C*--------------------------------------------------------------------------------------------*
050800970603     C     INZSTD        BEGSR
050900970603     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
051000970603     C                   CALL      'XKEYS'
051100970603     C                   PARM                    DSKEYS
051200970603     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
051300970603     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
051400970603     C     XMSGPA        PLIST
051500970603     C                   PARM                    DSMSMS
051600970603     C                   PARM                    DSMSPG
051700970603     C                   PARM                    DSMSKE
051800970603     C                   PARM                    DSMSER
051900970603     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
052000160715     C                   MOVEL     'TIS7VMS3R'   DSMSPG
052100160715     C                   MOVEL     'TIS7VMS3R'   VMCQUE
052200970603     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
052300970603     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
052400970603     C* INIZIALIZZO FLAG CONTROLLO PGM
052500970603     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
052600970603     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
052700970603     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
052800970603     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
052900970603     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
053000970603     C                   MOVEL     '0'           $FINE             1            *FINE PGM
053100970603     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
053200970603     C* LANCIO ROUTINE PER VERIFICARE SE
053300970603     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
053400970603     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
053500020919     C                   EXSR      REPDATIUTE
053600970603     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
053700970603     C                   BITOFF    '01234567'    POSMI1            1
053800970603     C                   BITOFF    '01234567'    POSMI2            1
053900970603     C                   BITOFF    '01234567'    POSMA1            1
054000970603     C                   BITOFF    '01234567'    POSMA2            1
054100970603     C                   BITON     '34'          POSMI1            1            *18
054200970603     C                   BITON     '7'           POSMI2            1            *01
054300970603     C                   BITON     '34'          POSMA1            1            *18
054400970603     C                   BITON     '0'           POSMA2            1            *80
054500970603     C                   MOVE      POSMI2        POSMIN            2
054600970603     C                   MOVEL     POSMI1        POSMIN
054700970603     C                   MOVE      POSMA2        POSMAX            2
054800970603     C                   MOVEL     POSMA1        POSMAX
054900970603     C                   ENDSR
055000970603     C*--------------------------------------------------------------------------------------------*
055100970603     C* FUNSTD - LANCIA SUBROUTINE STANDARD
055200970603     C*--------------------------------------------------------------------------------------------*
055300970603     C     FUNSTD        BEGSR
055400970603     C                   SELECT
055500970603     C     KEY           WHENEQ    F3
055600970603     C                   EXSR      F03STD
055700970603     C     KEY           WHENEQ    F12
055800970603     C                   EXSR      F12STD
055900970603     C     KEY           WHENEQ    F24
056000970603     C                   EXSR      F24STD
056100970603     C                   OTHER
056200160902     C                   MOVEL     MSG(2)        V1MSG
056300160902     C***                MOVEL     MSG(2)        $MSG
056400160902     C***                EXSR      CARMSG
056500970603     C                   ENDSL
056600970603     C                   ENDSR
056700970603     C*--------------------------------------------------------------------------------------------*
056800970603     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
056900970603     C*--------------------------------------------------------------------------------------------*
057000970603     C     F03STD        BEGSR
057100970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
057200970603     C                   MOVEL     '1'           $FINE
057300970603     C* IMPOSTA FLAG DI FINE PROCEDURA
057400970603     C                   MOVEL     '1'           $F03
057500970603     C* PULISCE IL MESSAGGIO
057600160715     C                   MOVEL     *BLANKS       VMS2MSG
057700970603     C                   ENDSR
057800970603     C*--------------------------------------------------------------------------------------------*
057900970603     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
058000970603     C*--------------------------------------------------------------------------------------------*
058100970603     C     F12STD        BEGSR
058200970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
058300970603     C                   MOVEL     '1'           $FINE
058400970603     C* IMPOSTA FLAG DI FINE PROCEDURA
058500970603     C                   MOVEL     '1'           $F12
058600970603     C* PULISCE IL MESSAGGIO
058700160715     C                   MOVEL     *BLANKS       VMS2MSG
058800970603     C                   ENDSR
058900970603     C*--------------------------------------------------------------------------------------------*
059000970603     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
059100970603     C*--------------------------------------------------------------------------------------------*
059200970603     C     F24STD        BEGSR
059300970603     C*
059400970603     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
059500970603     C                   EXSR      OUTFUN
059600970603     C*
059700970603     C                   ENDSR
059800970603     C*--------------------------------------------------------------------------------------------*
059900970603     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
060000970603     C*          CODA MESSAGGI DEL PGM
060100970603     C*--------------------------------------------------------------------------------------------*
060200970603     C     CARMSG        BEGSR
060300970603     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
060400970603     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
060500970603     C     MSGULT        IFEQ      0
060600970603     C                   MOVEL     '1'           *IN40
060700160715     C                   WRITE     S7V3DVK
060800970603     C                   MOVEL     '0'           *IN40
060900970603     C                   ENDIF
061000970603     C* INVIA MESSAGGIO ALLA CODA DEL PGM
061100970603     C                   MOVEL     $MSG          DSMSMS
061200970603     C                   CALL      'XMSG'        XMSGPA
061300970603     C     DSMSER        IFEQ      0
061400970603     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
061500970603     C                   ENDIF
061600970603     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
061700970603     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
061800160715     C                   WRITE     S7V3DVM                                      *SFL MESS
061900970603     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
062000970603     C                   MOVEL     *BLANKS       $MSG
062100970603     C                   ENDSR
062200970603     C*--------------------------------------------------------------------------------------------*
062300970603     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
062400970603     C*--------------------------------------------------------------------------------------------*
062500970603     C     OUTMSG        BEGSR
062600970603     C* VISUALIZZAZIONE MESSAGGIO
062700970603     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
062800160715     C                   WRITE     S7V3DVK                                      *CONTR MESS
062900970603     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
063000970603     C                   Z-ADD     *ZEROS        MSGULT
063100970603     C                   ENDSR
063200970603     C*--------------------------------------------------------------------------------------------*
063300970603     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
063400970603     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
063500970603     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
063600970603     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
063700970603     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
063800970603     C*--------------------------------------------------------------------------------------------*
063900970603     C     CARFUN        BEGSR
064000970603     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
064100970603     C     1             DO        31            I                 5 0          --- 1 -->
064200970603     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
064300970603     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
064400970603     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
064500020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
064600970603     C     $TGES         IFEQ      'G'                                          *GESTIONE
064700970603     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
064800970603     C                   ELSE                                                   *INTERROG.
064900970603     C                   MOVEL     FUNASI        FAT(I)
065000970603     C                   ENDIF
065100970603     C                   ELSE                                                   <-- 2 --> *FILIALE
065200970603     C     $TGES         IFEQ      'G'                                          *GESTIONE
065300970603     C                   MOVEL     FUNAFG        FAT(I)
065400970603     C                   ELSE                                                   *INTERROG.
065500970603     C                   MOVEL     FUNAFI        FAT(I)
065600970603     C                   ENDIF
065700970603     C                   ENDIF                                                  <-- 2 ---
065800970603     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
065900970603     C                   MOVEL     USER          WEDP              3            *PROFILO
066000970603     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
066100970603     C                   MOVEL     'S'           FAT(I)                         *ABILITO
066200970603     C                   ELSE
066300970603     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
066400970603     C                   ENDIF
066500970603     C                   ENDIF
066600970603     C                   ENDDO                                                  <-- 1 ---
066700970603     C                   ENDSR
066800970603     C*--------------------------------------------------------------------------------------------*
066900970603     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
067000970603     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
067100970603     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
067200970603     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
067300970603     C*--------------------------------------------------------------------------------------------*
067400970603     C     OUTFUN        BEGSR
067500970603     C     1             DO        8             I                              *PULIZIA FUNZIONI A
067600970603     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
067700970603     C                   ENDDO
067800970603     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
067900970603     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
068000970603     C     1             DO        31            I
068100970603     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
068200970603     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
068300970603     C                   ADD       1             FUNNAT
068400970603     C                   ENDIF
068500970603     C                   ENDDO
068600970603     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
068700970603     C     FUNNAT        IFGT      8
068800970603     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
068900970603     C                   ENDIF
069000970603     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
069100970603     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
069200970603     C                   Z-ADD     8             FUNVIS            5 0
069300970603     C     FAT(23)       IFEQ      'S'
069400970603     C                   SUB       1             FUNVIS
069500970603     C                   ENDIF
069600970603     C     FAT(24)       IFEQ      'S'
069700970603     C                   SUB       1             FUNVIS
069800970603     C                   ENDIF
069900970603     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
070000970603     C                   Z-ADD     *ZEROS        W                 5 0
070100970603     C                   ADD       1             FUNULT
070200970603     C     FUNULT        IFGT      31
070300970603     C                   Z-ADD     1             FUNULT
070400970603     C                   ENDIF
070500970603     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
070600970603     C     FUNULT        DO        31            I                              --- 1 -->
070700970603     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
070800970603     C     W             IFLT      FUNVIS                                       --- 2 -->
070900970603     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
071000970603     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
071100970603     C     FDE(I)        ANDNE     *BLANKS
071200970603     C     I             IFEQ      23                                           *TOLGO F23
071300970603     C     I             OREQ      24                                           *E F24
071400970603     C                   ELSE
071500970603     C                   ADD       1             W
071600970603     C                   MOVEL     FDE(I)        FUV(W)
071700970603     C                   ENDIF
071800970603     C                   ENDIF                                                  <-- 3 ---
071900970603     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
072000970603     C                   Z-ADD     I             FUNULT
072100970603     C                   ENDIF                                                  <-- 2 ---
072200970603     C                   ENDDO                                                  <-- 1 ---
072300970603     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
072400970603     C                   MOVEL     FUV(1)        VFD001
072500970603     C                   MOVEL     FUV(2)        VFD002
072600970603     C                   MOVEL     FUV(3)        VFD003
072700970603     C                   MOVEL     FUV(4)        VFD004
072800970603     C                   MOVEL     FUV(5)        VFD005
072900970603     C                   MOVEL     FUV(6)        VFD006
073000970603     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
073100970603     C     FAT(24)       ANDEQ     'N'
073200970603     C                   MOVEL     FUV(7)        VFD007
073300970603     C                   MOVEL     FUV(8)        VFD008
073400970603     C                   ELSE                                                   <-- 1 -->
073500970603     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
073600970603     C     FAT(24)       ANDEQ     'S'
073700970603     C                   MOVEL     FDE(23)       VFD007
073800970603     C                   MOVEL     FDE(24)       VFD008
073900970603     C                   ELSE                                                   <-- 2 -->
074000970603     C                   MOVEL     FUV(7)        VFD007
074100970603     C     FAT(23)       IFEQ      'S'
074200970603     C                   MOVEL     FDE(23)       VFD008
074300970603     C                   ELSE
074400970603     C                   MOVEL     FDE(24)       VFD008
074500970603     C                   ENDIF
074600970603     C                   ENDIF                                                  <-- 2 ---
074700970603     C                   ENDIF                                                  <-- 1 ---
074800970603     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
074900970603     C                   ENDSR
075000970603     C*--------------------------------------------------------------------------------------------*
075100970603     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
075200970603     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
075300970603     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
075400970603     C*                   - ERRORE SE FUNZIONE NON ATTIVA
075500970603     C*--------------------------------------------------------------------------------------------*
075600970603     C     CONFUN        BEGSR
075700970603     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
075800970603     C     1             DO        31            I                              --- 1 -->
075900970603     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
076000970603     C* CONTROLLO SE IL TASTO E' ATTIVO
076100970603     C     FAT(I)        IFNE      'S'                                          --- 3 -->
076200160902     C                   MOVEL     MSG(1)        V1MSG                          *FUNZ.NON ACCETTABIL
076300160902     C***                MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
076400160902     C***                EXSR      CARMSG
076500970603     C                   MOVEL     '1'           $ERR
076600970603     C                   ENDIF                                                  <-- 3 ---
076700970603     C                   ENDIF                                                  <-- 2 ---
076800970603     C                   ENDDO                                                  <-- 1 ---
076900970603     C                   ENDSR
077000970603     O*--------------------------------------------------------------------------------------------*
077100970603** -MSG- (MESSAGGI)
077200160715TIS7VMS3R- Tasto funzionale non valido.                                        1
077300160715TIS7VMS3R- Tasto funzionale abilitato ma non gestito. Avvertire CED.           2
077400160715TIS7VMS3R- Opzione non valida.                                                 3
077500160715TIS7VMS3R- Opzione abilitata ma non gestita. Avvertire CED.                    4
077600160715TIS7VMS3R- Inizio elenco.                                                      5
077700160715TIS7VMS3R- Fine elenco.                                                        6
077800160715TIS7VMS3R- Campo obbligatorio                                                  7
077900160715TIS7VMS3R- Data errata                                                         8
078000160715TIS7VMS3R- La data scadenza deve essere maggiore della decorrenza              9
078100160715TIS7VMS3R- Versione già inserita                                              10
078200160902TIS7VMS3R- La versione non può sovrapporsi ad una già in decorrenza           11
078300160715TIS7VMS3R- La versione non può scadere in un giorno già trascorso             12
078400160715TIS7VMS3R- 13
078500160715TIS7VMS3R- 14
078600160715TIS7VMS3R- 15
078700160715TIS7VMS3R- 16
078800160715TIS7VMS3R- 17
078900160715TIS7VMS3R- 18
079000160715TIS7VMS3R- Errori in entrata duplica. Non trovato il record da cui copiare    19
079100160715TIS7VMS3R- Duplicazione effettuata                                            20
079200970603** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
079300970603F1    -                   -    NN     NN
079400970603F2    -                   -    NN     NN
079500970603F3    -F3=Fine            -    NN     NN
079600970603F4    -                   -    NN     NN
079700970603F5    -                   -    NN     NN
079800970603F6    -F6=Conferma        -    SN     SN
079900970603F7    -                   -    NN     NN
080000970603F8    -                   -    NN     NN
080100970603F9    -                   -    NN     NN
080200970603F10   -                   -    NN     NN
080300970603F11   -                   -    NN     NN
080400970603F12   -F12=Ritorno        -    SS     SS
080500970603F13   -                   -    NN     NN
080600970603F14   -                   -    NN     NN
080700970603F15   -                   -    NN     NN
080800970603F16   -                   -    NN     NN
080900970603F17   -                   -    NN     NN
081000970603F18   -                   -    NN     NN
081100970603F19   -                   -    NN     NN
081200970603F20   -                   -    NN     NN
081300970603F21   -                   -    NN     NN
081400970603F22   -                   -    NN     NN
081500970603F23   -                   -    NN     NN
081600970603F24   -F24=Altri tasti    -    NN     NN
081700970603ENTER -                   -    SS     SS
081800970603ROLLUP-                   -    NN     NN
081900970603ROLLDN-                   -    NN     NN
082000970603HELP  -                   -    NN     NN
082100970603PRINT -                   -    NN     NN
082200970603BSPACE-                   -    NN     NN
082300970603CLEAR -                   -    NN     NN
