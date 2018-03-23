000100900423     H* FLFG06R *----------------------------------------------------*
000200000000     H*-----------------*                                            *
000300900508     H*         - STAMPA MODULO  AL MITTENTE - FILTRO -              *
000400900423     H*         ---------------------------------------              *
000500000000     H*--------------------------------------------------------------*
000600940921     FFNLG06D   CF   E             WORKSTN
000700911031     FTABEL00F  IF   E           K DISK
000800010528     FAZORG01L  IF   E           K DISK
000900941223     FFNLV22P   O    F  132        PRINTER USROPN
001000940921     D*--------------------------------------------------------------*
001100961128     D FL£7            S              3    DIM(29)
001200020430     D £1              S              3  0 DIM(30)
001300961128     D £7              S              3  0 DIM(29)
001400970204     D C2F             S              1    DIM(100)
001500950124     D CCO             S              1    DIM(80) CTDATA PERRCD(80)
001600991206     D ODP             S              1    DIM(80) CTDATA PERRCD(80)
001700030304     D OFe             S              1    DIM(80) CTDATA PERRCD(80)
001800950124     D CME             S              1    DIM(80) CTDATA PERRCD(80)
001900941007     D DOV             S              1    DIM(70) CTDATA PERRCD(70)
002000030717     D CM4             S              1    DIM(80) CTDATA PERRCD(80)
002100030717     D CM5             S              1    DIM(80) CTDATA PERRCD(80)
002200031027     D ERR             S             70    DIM(5) CTDATA PERRCD(1)
002300940921     D*--------------------------------------------------------------*
002400940922     D WLBDA8          DS
002500940922     D  G02DAT                 1      8  0
002600940922     D  G02INV                 9     16  0
002700940922     D  G02ERR                17     17
002800940922     D  G02TGI                18     22  0
002900940922     D FNLG07        E DS                  EXTNAME(FNLG07DS)
003000940922     D DS2F          E DS
003100961127     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003200961127     D CNCR80        E DS
003300020430     D TR06DS        E DS                  EXTNAME(TRUL06DS)
003400020430     D  LIN                    1     90  0 dim(30)
003500900131     D KPJBA         E DS
003600900131     D  LIBSYS                92    101
003700900131     D  MSGERR               453    502
003800020926      *parametri per reperimento coda
003900020926     d finep           s              1
004000020926     d codpo           s              3s 0
004100020926     d tpcoda          s              1
004200020926     d nomcoda         s             10
004300020926     d libcoda         s             10
004400020926     d esito           s              1
004500900131     C*---------------------------------------------------------------*
004600940921     C*  Inizializzazione prima videata
004700940921     C                   EXSR      INZ01
004800940921     C*  loop prima videata
004900940921     C     *INKC         DOWEQ     '0'
005000940921     C     *INKF         ANDEQ     '0'
005100940921     C                   EXSR      GESD01
005200940921     C                   END
005300000000     C     FINE          TAG
005400940921     C                   SETON                                        LR
005500940921     C*-----------------------------------------------------*
005600940921     C*  Gestione della prima videata
005700940921     C*------------------------------------------------------*
005800940921     C     GESD01        BEGSR
005900940921     C                   EXFMT     LG06D01
006000940921     C                   SETOFF                                       284021
006100961128     C                   SETOFF                                       0241
006200940921     C   KC              GOTO      FINVD1
006300961127     C*   SE RICHIESTO CAMBIO FILIALE VADO A FINE VIDEATA
006400961127     C     *INKS         IFEQ      '1'
006500961127     C                   SETON                                        1802
006600961127     C                   GOTO      FINVD1
006700961127     C                   END
006800961128     C*   Se ho modificatao la filiale controllo nuovo valore
006900020730     C     SimPOU        IFNE      V1CFGS
007000961128     C                   EXSR      CTRFIL
007100961128     C                   END
007200940921     C*§  RICERCA IN TABEL00F
007300940922     C     V1CTIP        IFEQ      '?'
007400940921     C                   MOVEL     '2F'          §COD              2
007500940921     C                   MOVEL     *BLANK        §KEY              8
007600940921     C                   MOVEL     *BLANK        §DES             35
007700940922     C                   MOVE      *BLANK        V1CTIP
007800940921     C                   CALL      'X§TABER'
007900940921     C                   PARM                    CODUT
008000940921     C                   PARM                    §COD
008100940921     C                   PARM                    §KEY
008200940921     C                   PARM                    §DES
008300940922     C                   MOVEL     §KEY          V1CTIP
008400940921     C                   END
008500940922     C     V1CTIP        IFNE      *BLANKS
008600940921     C                   MOVEL     '2F'          TBLCOD
008700940921     C                   MOVEL     *BLANK        TBLKEY
008800940922     C                   MOVEL     V1CTIP        TBLKEY
008900940921     C     KTAB          CHAIN     TABEL                              21
009000940921     C     *IN21         IFEQ      '1'
009100940921     C                   SETON                                        2840
009200940921     C                   MOVEL     ERR(1)        $MSG
009300940922     C                   ELSE
009400940922     C                   MOVEL     TBLUNI        DS2F
009500940922     C     §2FFTM        IFEQ      'N'
009600940922     C                   SETON                                        2840
009700940922     C                   MOVEL     ERR(3)        $MSG
009800940922     C                   GOTO      FINVD1
009900940922     C                   END
010000031027     C     §2Fris        IFne      *blank
010100031027     C                   SETON                                        2840
010200031027     C                   MOVEL     ERR(5)        $MSG
010300031027     C                   GOTO      FINVD1
010400031027     C                   END
010500020730     C                   IF        (SimTpP = '2' OR SimTpP = *BLANK)
010600020730     C                             AND
010700020730     C                             §2FFTP = 'P'
010800940922     C                   SETON                                        2840
010900940922     C                   MOVEL     ERR(2)        $MSG
011000940922     C                   END
011100940921     C                   END
011200940922     C                   END
011300961127     C* SE è stato modificato il codice della filiale lo controllo
011400020730     C     SimPOU        IFNE      V1CFGS
011500961128     C                   EXSR      CTRFIL
011600961127     C                   END
011700940921     C* PASSAGGIO PARAMETRI IN KBJBU E LANCIO BATCH DI STAMPA LETTERE
011800940921     C     *INKF         IFEQ      '1'
011900941007     C     *IN28         ANDEQ     '0'
012000970204     C* Se sono una secondo livello richiamo pgm iterattivamente
012100970204     C* perchè posso stampare solo fli invii manuali:
012200970204     C     *IN01         IFEQ      '0'
012300970204     C                   EXSR      CALL07
012400970204     C                   ELSE
012500970204     C* Se ho selezionato un tipo invio...
012600970204     C     V1CTIP        IFNE      *BLANKS
012700970204     C* .. controllo se è manuale...
012800970204     C     V1CTIP        LOOKUP    C2F                                    32
012900970204     C     *IN32         IFEQ      '1'
013000970204     C* .. e se si richiamo pgm iterattivamente
013100970204     C                   EXSR      CALL07
013200970204     C                   ELSE
013300970204     C* .. altrimenti eseguo sottomissione
013400970204     C                   EXSR      SBMP07
013500970204     C                   END
013600970204     C* Se non ho selezionato un tipo invio e sono una 1° livello...
013700970204     C* e sottometto la stampa degli altri tipi comunicazione
013800970204     C                   ELSE
013900970204     C* .. richiamo iterrattivamente la stp.moduli con invio manuale
014000970204     C                   DO        WY            T                 3 0
014100970204     C                   MOVEL     C2F(T)        V1CTIP
014200970204     C                   EXSR      CALL07
014300970204     C                   END
014400970204     C* ... e sottometto la stampa degli altri tipi comunicazione
014500970204     C                   MOVEL     *BLANKS       V1CTIP
014600970204     C                   EXSR      SBMP07
014700970204     C                   END
014800970204     C                   END
014900970204     C*
015000970204     C                   END
015100970204     C*
015200970204     C     FINVD1        ENDSR
015300970204     C*-----------------------------------------------------*
015400970204     C*  Call a FNLG07R
015500970204     C*------------------------------------------------------*
015600970204     C     CALL07        BEGSR
015700941007     C* RICH. IMMISS. BOLLE
015800030717     C                   MOVEL     *BLANKS       D07TPL
015900030717     C                   Z-ADD     80            LUNCMD           15 5
016000030717     C* OVRPRTF  BOLLE OLD
016100941007     C                   MOVEA     D07PRB        CCO(28)
016200950124     C                   MOVEA     D07MOB        CCO(61)
016300941007     C                   CALL      'QCMDEXC'
016400941007     C                   PARM                    CCO
016500941007     C                   PARM                    LUNCMD
016600030717     C* OVRPRTF  BOLLE A4
016700030717     C                   MOVEA     D07PRB4       CM4(30)
016800030721     C                   MOVEA     D07MDB4       CM4(63)
016900030717     C                   CALL      'QCMDEXC'
017000030717     C                   PARM                    CM4
017100030717     C                   PARM                    LUNCMD
017200030717     C* OVRPRTF  BOLLE A5
017300030717     C                   MOVEA     D07PRB5       CM5(30)
017400030721     C                   MOVEA     D07MDB5       CM5(63)
017500030717     C                   CALL      'QCMDEXC'
017600030717     C                   PARM                    CM5
017700030717     C                   PARM                    LUNCMD
017800991206     C* OVRPRTF  BOLLE DPD
017900991206     C                   MOVEA     D07PRB        ODP(29)
018000991206     C                   CALL      'QCMDEXC'
018100991206     C                   PARM                    ODP
018200991206     C                   PARM                    LUNCMD
018300030304     C* OVRPRTF  BOLLE FedEx
018400030307     c                   Movel     *Blanks       comman           90
018500030307     C                   Z-ADD     90            LUNCMD           15 5
018600030304     C                   MOVEA     D07PRB        OFe(29)
018700030307     c                   Movea     Ofe           Comman
018800030307     c                   Eval      %Subst(Comman:74:9) = 'COPIES(2)'
018900030304     C                   CALL      'QCMDEXC'
019000030307     c                   PARM                    comman
019100030304     C                   PARM                    LUNCMD
019200941007     C* OVRPRTF  ETICHETTE
019300950124     C                   Z-ADD     80            LUNCMD           15 5
019400941007     C                   MOVEA     D07PRE        CME(28)
019500950124     C                   MOVEA     D07MOE        CME(61)
019600941007     C                   CALL      'QCMDEXC'
019700941007     C                   PARM                    CME
019800941007     C                   PARM                    LUNCMD
019900961202     C                   Z-ADD     V1CFGS        D07FIL
020000940922     C                   MOVE      V1CTIP        D07TIP
020100940922     C                   MOVE      V1CRAC        D07RAC
020200020926     C                   MOVE      nomcoda       D07PRF
020300940922     C                   MOVEL     FNLG07        KPJBU
020400960202     C                   CALL      'FNLG07C'
020500940921     C                   PARM                    KPJBA
020600941007     C* MI RITORNA IL NR. BOLLA CREATO
020700941007     C* DLTOVR
020800941007     C                   Z-ADD     70            LUNCMD           15 5
020900941007     C                   CALL      'QCMDEXC'
021000941007     C                   PARM                    DOV
021100941007     C                   PARM                    LUNCMD
021200940921     C*
021300970204     C                   ENDSR
021400970204     C*-----------------------------------------------------*
021500970204     C*  Sbmp a FNLG07R
021600970204     C*------------------------------------------------------*
021700970204     C     SBMP07        BEGSR
021800970204     C*
021900970206     C                   MOVEL     'B'           D07TPL
022000970204     C                   Z-ADD     V1CFGS        D07FIL
022100970204     C                   MOVE      V1CTIP        D07TIP
022200970204     C                   MOVE      V1CRAC        D07RAC
022300020926     C                   MOVE      nomcoda       D07PRF
022400970204     C                   MOVEL     FNLG07        KPJBU
022500970204     C                   MOVEL     'LG07'        KCOAZ
022600170221     C                   CALL      'FNLG07C'
022700970204     C                   PARM                    KPJBA
022800970204     C*
022900970204     C                   ENDSR
023000940921     C*-----------------------------------------------------*
023100940921     C*  Inizializzazione videata
023200940921     C*------------------------------------------------------*
023300940921     C     INZ01         BEGSR
023400961127     C*
023500020730     C                   MOVEL     SimPOU        V1CFGS
023600940922     C                   MOVEL     'S'           V1CRAC
023700020730     C                   IF        SimTpP = '2' OR SimTpP = *BLANK
023800940923     C                   MOVEL     'N'           V1CRAC
023900940923     C                   END
024000940922     C                   MOVEL     *BLANKS       V1CTIP
024100961128     C*  Caricamento tabella £7
024200961128     C                   EXSR      CAR£7
024300961128     C*
024400961128     C                   ENDSR
024500961128     C*----------------------------------------------------------*
024600961128     C* Controllo filiale
024700961128     C*----------------------------------------------------------*
024800961128     C     CTRFIL        BEGSR
024900961128     C*
025000961128     C     V1CFGS        LOOKUP    £1                                     32
025100961128     C     *IN32         IFEQ      '0'
025200961128     C                   SETON                                        2841
025300961128     C                   MOVEL     ERR(4)        $MSG
025400961128     C                   ELSE
025500961128     C                   EXSR      CAR£7
025600961128     C                   END
025700961128     C*
025800961128     C                   ENDSR
025900961128     C*----------------------------------------------------------*
026000961128     C* Caricamento tabella £7
026100961128     C*----------------------------------------------------------*
026200961128     C     CAR£7         BEGSR
026300961128     C* Caricamento £7
026400961128     C                   MOVEA     *ZEROS        FL£7
026500961128     C                   MOVEL     *BLANKS       TBLKEY
026600961128     C                   MOVEL     V1CFGS        TBLKEY
026700961128     C                   MOVEL     '£7'          TBLCOD
026800961128     C     KTAB          CHAIN     TABEL                              31
026900961128     C     *IN31         IFEQ      '0'
027000961128     C     TBLFLG        ANDEQ     ' '
027100961128     C                   MOVEA     TBLUNI        FL£7
027200961128     C                   ELSE
027300961128     C                   MOVE      V1CFGS        FL£7(1)
027400961128     C                   END
027500961128     C*
027600961128     C                   Z-ADD     0             £7
027700961128     C                   Z-ADD     0             X£7               2 0
027800961128     C                   DO        29            X                 3 0
027900961128     C     FL£7(X)       IFNE      *BLANKS
028000961128     C                   ADD       1             X£7               2 0
028100961128     C                   MOVE      FL£7(X)       £7(X£7)
028200961128     C                   END
028300961128     C                   END
028400961128     C*
028500961128     C                   ENDSR
028600940921     C*-----------------------------------------------------*
028700940921     C*  Operazioni iniziali
028800940921     C*------------------------------------------------------*
028900940921     C     *INZSR        BEGSR
029000940921     C*
029100940921     C     *ENTRY        PLIST
029200940921     C                   PARM                    KPJBA
029300941010     C                   MOVEL     KPJBU         FNLG07
029400940921     C                   Z-ADD     1             CODUT
029500961127     C                   CALL      'X§PARUT'
029600961127     C                   PARM                    UTEDSE
029700961127     C                   MOVEL     REC80         CNCR80
029800940921     C     CODUT         CABEQ     -1            FINE
029900940922     C                   MOVEL     RAGUT         V1CRSU
030000020730     C                   EVAL      *IN01 = (SimTpP <> '2')
030100940921     C*
030200940921     C     KTAB          KLIST
030300940921     C                   KFLD                    CODUT
030400940921     C                   KFLD                    TBLCOD            2
030500940921     C                   KFLD                    TBLKEY            8
030600970204     C     KTAB1         KLIST
030700970204     C                   KFLD                    CODUT
030800970204     C                   KFLD                    TBLCOD            2
030900940922     C*----------------------------------------------------*
031000940922     C* Pulizia dati DS
031100941010     C                   Z-ADD     0             D07AGC
031200941010     C                   Z-ADD     0             D07FGC
031300941010     C                   Z-ADD     0             D07NGC
031400940922     C                   MOVEL     'N'           D07RIS
031500940922     C                   TIME                    WHHDAT           14 0
031600940922     C                   MOVE      WHHDAT        G02DAT
031700940922     C                   MOVE      *ZEROS        G02INV
031800940922     C                   MOVEL     *BLANK        G02ERR
031900940922     C                   CALL      'XSRDA8'
032000940922     C                   PARM                    WLBDA8
032100940922     C                   Z-ADD     G02INV        D07DAT            8 0
032200940922     C* Finto open X rendere SHARE *YES  i file
032300941223     C                   OPEN      FNLV22P
032400940922     C   LR
032500940922     CANNLR              EXCEPT    FINTO
032600970204     C*
032700970204     C* Caricamento 2F
032800970204     C                   Z-ADD     0             WY                3 0
032900970204     C                   MOVEA     *HIVAL        C2F
033000970204     C                   MOVEL     '2F'          TBLCOD
033100970204     C     KTAB1         CHAIN     TABEL                              31
033200970204     C     *IN31         DOWEQ     '0'
033300970204     C     TBLFLG        IFEQ      ' '
033400970204     C                   MOVEL     TBLUNI        DS2F
033500970204     C     §2FFTP        IFEQ      'B'
033600970204     C                   ADD       1             WY
033700970204     C                   MOVEL     TBLKEY        C2F(WY)
033800970204     C                   END
033900970204     C                   END
034000970204     C     KTAB1         READE     TABEL                                  31
034100970204     C                   END
034200971202     C                   Z-ADD     0             Y                 2 0
034300020926      *reperimento coda fax sostituisce tabella 3i
034400020926     c                   move      'L'           finep
034500020926     c                   z-add     simpou        codpo
034600020926     c                   move      '2'           tpcoda
034700020926     c                   clear                   nomcoda
034800020926     c                   clear                   libcoda
034900020926     c                   clear                   esito
035000020926     c                   call      'TRULOUTQ'
035100020926     c                   parm                    finep
035200020926     c                   parm                    codpo
035300020926     c                   parm                    tpcoda
035400020926     c                   parm                    nomcoda
035500020926     c                   parm                    libcoda
035600020926     c                   parm                    esito
035700020927     c                   if        esito <> '0' or nomcoda = *blank
035800020927     c                   eval      nomcoda = 'QDKT'
035900020927     c                   endif
036000961128     C*
036100961128     C* Caricamento £1
036200020430     C                   CLEAR                   TR06DS
036300020430     C                   MOVEL     '£1'          D06COD
036400020730     C                   MOVEL     SimPOU        D06KEY
036500020430     C                   MOVEL     TR06DS        KPJBU
036600020430     C                   CALL      'TRUL06R'
036700020430     C                   PARM                    KPJBA
036800020430     C                   MOVEL     KPJBU         TR06DS
036900020430     C                   MOVEA     LIN           £1
037000961128     C*
037100940922     C                   ENDSR
037200940922     C*----------------------------------------------------*
037300941223     OFNLV22P   E            FINTO
037400030717** CCO
037500030721OVRPRTF FILE(FNLSB0P) OUTQ(          ) SAVE(*YES) FORMTYPE('1234567890')
037600991206** ODP
037700991206OVRPRTF FILE(FNLSB0P1) OUTQ(          ) SAVE(*YES) FORMTYPE('BOLLE-dpd')
037800030304** OFE
037900030304OVRPRTF FILE(FNLSB0P2) OUTQ(          ) SAVE(*YES) FORMTYPE('BOLLE-Fed')
038000941007** CME
038100950124OVRPRTF FILE(FNLV22P) OUTQ(          ) SAVE(*YES) FORMTYPE('          ')
038200941007** DOV
038300941007DLTOVR  FILE(*ALL)
038400030717** CM4
038500030721OVRPRTF FILE(FNLSB5PA4) OUTQ(          ) SAVE(*YES) FORMTYPE('          ')
038600030717** CM5
038700030721OVRPRTF FILE(FNLSB5PA5) OUTQ(          ) SAVE(*YES) FORMTYPE('          ')
038800940921**
038900940921Tipo comunicazione non presente in tabella
039000961113Tipo comunicazione non ammesso per punto operativo secondo livello
039100940922Tipo di comunicazione richiesto non prevede la stampa del mod.Mittente
039200961128Punto operativo non gestito su questo AS
039300031027Tipo comunicazione non ammesso
