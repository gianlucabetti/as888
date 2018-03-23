000100970526     H*------------------------------------------------------------------------*
000200021218     H* "DRIVER" MOVIMENTI GABBIE E BANCALI
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800021218     FAZMGB00F  O  A E             DISK
000900150226     Fazmgb05l  uF   E           K DISK    rename(azmgb000:azmgb5) prefix(w5)
001000021218     FTNTBE01L  IF   E           K DISK
001100021218     FCNACO00F  IF   E           K DISK
001200021218     FAZDST04L  IF   E           K DISK
001300021218     FAZORG01L  IF   E           K DISK
001400021218     FAZAGB01L  IF   E           K DISK
001500150220     FAZcGB01L  IF   E           K DISK
001600150216     Ffnspe01l  IF   E           K DISK
001700970526     D*------------------------------------------------------------------------*
001800970526     D* INPUT
001900970526     D*------------------------------------------------------------------------*
002000021218     D* DS "XSRDA8" - CONTROLLA DATA (8)
002100021218     D*------------
002200021218     D WLBDA8          DS                  INZ
002300021218     D  G08DAT                 1      8  0
002400021218     D  G08INV                 9     16  0
002500021218     D  G08ERR                17     17
002600021218     D  G08TGI                18     22  0
002700021218     D*------------
002800021218     D* DS DI MEMORIZZAZIONE MOVIMENTI DA INSERIRE
002900021218     D*------------
003000021219     D AZMGBXXX      E DS                  EXTNAME(AZMGB00F)
003100021218     D AZMGB001      E DS                  EXTNAME(AZMGB00F) PREFIX(W1_)
003200021218     D AZMGB002      E DS                  EXTNAME(AZMGB00F) PREFIX(W2_)
003300021218     D*------------
003400020412     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
003500021218     D*------------
003600021218     D TISIG0DS      E DS
003700021218     D*------------
003800021218     D* DS CHIAMAMTA *PGM REPERIMENTO DATI ALTRO UTENTE (TIBS36R)
003900021218     D*------------
004000021218     D TIBS36DS      E DS
004100021218     D*------------
004200021218     D* DS RIDEFINIZIONE CAMPO DATI UTENTE DELLA DS TIBS36DS RELATIVO ALLA STRUTTIRA DI AZUTE00F
004300021218     D*------------
004400021218     D AZUTE00F      E DS
004500021218     D*------------
004600021218     D* DS RIDEFINIZIONE CAMPO AUTORIZZAZIONI SPECIALI (UTEFAF) DI AZUTE00F
004700021218     D*------------
004800150220     d Tibs69DS      E DS                  INZ
004900150220     d DS_cnaco      E DS                  extname(CNACO00F)
005000150220     d DS_cnind      E DS                  extname(CNIND00F)
005100150220     d DS_cnclp      E DS                  extname(CNCLP00F)
005200150220     d DS_fncls      E DS                  extname(FNCLS00F)
005300150220      *
005400150205     D DBLP          E DS
005500150205     D DTASv         E DS
005600150205     D trul90ds      E DS                  inz
005700150226     d   D90RSE      e                     inz('S')
005800150226     d   D90RSB      e                     inz('S')
005900021218     D*------------
006000021218     D* DS X LETTURA NUOVA TABELLA GGB
006100021218     D*------------
006200021218     D DGGB          E DS
006300150220     D DTBA          E DS
006400030122     D*------------------
006500030122     D* DS X REPERIMENTO NUMERATORE
006600030122     D*------------------
006700030122     D TRUL33DS      E DS                  INZ
006800030122     D*------------------
006900030122     D* ARCHITETTURA
007000030122     D*------------------
007100030122     D KPJBA         E DS
007200030122     D*------------------
007300021218     D* SCHIERE
007400030122     D*------------------
007500021218     D KCAU            S              4    DIM(100)                             *CHIAVE CAUSALE
007600021218     D DCAU            S                   LIKE(DGGB) DIM(100)                  *DATI CAUSALE
007700020415     D*------------------
007800020415     D* VARIABILI DI WRK
007900020415     D*------------------
008000021218     D X               S              3  0 INZ(*ZEROS)
008100021218     D ERRL00          S              1    INZ('0')
008200030122     D ERRNUM          S              1    INZ('0')
008300021218     D ERR_1GIRO       S              1    INZ('0')
008400021218     D ERR_2GIRO       S              1    INZ('0')
008500021218     D W_CAU           S                   LIKE(IG0CAU)
008600021218     D W_SOG           S                   LIKE(IG0SOG)
008700150325      *---------------------------------------------------------------
008800150325     D CMA4f6          S              1    DIM(130) CTDATA PERRCD(65)
008900150325     D CMA5f6          S              1    DIM(130) CTDATA PERRCD(65)
009000150325     D CMETf6          S              1    DIM(130) CTDATA PERRCD(65)
009100970526     C*------------------------------------------------------------------------*
009200970526     C* MAIN LINES
009300970526     C*------------------------------------------------------------------------*
009400970526     C*
009500021218     C                   IF        IG0TLA <> 'C'
009600020415     C                   EXSR      PROCEDI
009700021218     C                   EXSR      WRIMOV
009800020415     C                   ENDIF
009900020415     C*
010000970526     C* FINE PGM
010100021218     C                   IF        IG0TLA = 'E'
010200020415     C                   SETON                                        RT
010300020415     C                   ELSE
010400020415     C                   SETON                                        LR
010500020415     C                   ENDIF
010600970526     C*------------------------------------------------------------------------*
010700021218     C* PROCEDI - ELABORA E INSERISCE I MOVIMENTI GABBIE/BANCALI
010800970526     C*------------------------------------------------------------------------*
010900020415     C     PROCEDI       BEGSR
011000021218     C*
011100021218     C* INIZIALIZZO I CAMPI DI OUT DELLA DS DI PROCEDURA (TISIG0DS)
011200021218     C                   EVAL      OG0ERR = *blanks
011300021218     C                   EVAL      OG0MSG = *blanks
011400021218     C*--------------
011500021218     C* 1° GIRO: INSERIMENTO MOVIMENTO RICHIESTO
011600021218     C*--------------
011700021218     C                   MOVEL     '0'           ERR_1GIRO
011800021218     C*
011900021218     C* - EFFETTUO CONTROLLI FORMALI
012000021218     C                   EVAL      W_CAU = IG0CAU
012100021218     C                   EVAL      W_SOG = IG0SOG
012200021218     C                   EXSR      DECODI
012300021218     C*
012400021218     C* - SE TUTTO OK RIEMPIO IL BUFFER X IL MOVIMENTO RICHIESTO
012500021218     C                   IF        ERRL00 = '0'
012600021218     C                   CLEAR                   AZMGB001
012700021218     C                   EVAL      W1_mgbTIP = §GGBTIP
012800021218     C                   EVAL      W1_mgbSOG = W_SOG
012900021218     C                   EVAL      W1_mgbCAU = W_CAU
013000021218     C                   EVAL      W1_mgbTIM = §GGBTIM
013100021218     C                   EVAL      W1_mgbCOD = IG0COD
013200021230     C* A seconda del segno della causale gestisco a programma il segno della quantità
013300021230     C                   IF        §GGBSEG = 'E'
013400021230     C                   EVAL      W1_mgbQTA = %abs(IG0QTA) * +1
013500021230     C                   ENDIF
013600021230     C                   IF        §GGBSEG = 'U'
013700021230     C                   EVAL      W1_mgbQTA = %abs(IG0QTA) * -1
013800021230     C                   ENDIF
013900030122     C                   IF        §GGBSEG = ' '
014000030122     C                   EVAL      W1_mgbQTA = IG0QTA
014100030122     C                   ENDIF
014200021230     C*
014300021218     C                   EVAL      W1_mgbRDO = IG0RDO
014400021218     C                   EVAL      W1_mgbDDO = IG0DDO
014500021218     C                   EVAL      W1_mgbUTE = IG0UTE
014600021218     C                   EVAL      W1_mgbPOU = IG0POU
014700021218     C                   EVAL      W1_mgbDAT = IG0DAT
014800021218     C                   EVAL      W1_mgbORA = IG0ORA
014900021218     C                   ELSE
015000021218     C                   MOVEL     '1'           ERR_1GIRO
015100021218     C                   ENDIF
015200021218     C*--------------
015300021218     C* 2° GIRO: SE NO ERRORI GIRO 1 E PREVISTO MOVIMENTO CORRELATO (SU CAUSALE 1° GIRO)
015400021218     C*--------------
015500021218     C                   MOVEL     '0'           ERR_2GIRO
015600021218     C*
015700021218     C                   IF        ERRL00 = '0' AND
015800021218     C                             §GGBCAC <> *blanks
015900021218     C*
016000021218     C* - EFFETTUO CONTROLLI FORMALI
016100021218     C                   EVAL      W_CAU = §GGBCAC
016200021218     C                   EVAL      W_SOG = IG0SMC
016300021218     C                   EXSR      DECODI
016400021218     C*
016500021218     C* - SE TUTTO OK RIEMPIO IL BUFFER X IL MOVIMENTO CORRELATO
016600021218     C                   IF        ERRL00 = '0'
016700021218     C                   CLEAR                   AZMGB002
016800021218     C                   EVAL      W2_mgbTIP = §GGBTIP
016900021218     C                   EVAL      W2_mgbSOG = W_SOG
017000021218     C                   EVAL      W2_mgbCAU = W_CAU
017100021218     C                   EVAL      W2_mgbTIM = §GGBTIM
017200021218     C                   EVAL      W2_mgbCOD = IG0COD
017300021230     C                   EVAL      W2_mgbQTA = W1_mgbQTA * -1
017400021218     C                   EVAL      W2_mgbRDO = IG0RDO
017500021218     C                   EVAL      W2_mgbDDO = IG0DDO
017600021218     C                   EVAL      W2_mgbUTE = IG0UTE
017700021218     C                   EVAL      W2_mgbPOU = IG0POU
017800021230     C                   EVAL      W2_mgbDAT = IG0DAT
017900021218     C                   EVAL      W2_mgbORA = IG0ORA
018000021218     C                   ELSE
018100021218     C                   MOVEL     '1'           ERR_2GIRO
018200021218     C                   ENDIF
018300021218     C                   ENDIF
018400020415     C*
018500970526     C                   ENDSR
018600021218     C*------------------------------------------------------------------------*
018700021218     C* DECODI - VERIFICA CORRETTEZZA DATI DI PASSAGGIO X GENERAZIONI MOVIMENTI
018800021218     C*------------------------------------------------------------------------*
018900021218     C     DECODI        BEGSR
019000021218     C*
019100021218     C* INIALIAZIZZO FLAG ERRORE
019200021218     C                   MOVEL     '0'           ERRL00
019300021218     C*
019400021218     C* CONTROLLO ESISTENZA CODICE CAUSALE
019500021218     C                   IF        ERRL00 = '0'
019600021230     C                   Z-ADD     1             X
019700021218     C     W_CAU         LOOKUP    KCAU(X)                                55
019800021218     C                   IF        *IN55 = *OFF
019900021218     C                   MOVEL     '1'           ERRL00
020000021218     C                   EVAL      OG0ERR = 'E'
020100021218     C                   EVAL      OG0MSG = 'CAUSALE INESISTENTE'
020200021218     C                   ELSE
020300021218     C                   MOVEL(P)  DCAU(X)       DGGB
020400021218     C                   ENDIF
020500021218     C                   ENDIF
020600021218     C*
020700021218     C* CONTROLLO ESISTENZA SOGGETTO MOVIMENTO
020800021218     C                   IF        ERRL00 = '0'
020900021218     C                   IF        §GGBTIP = 'C'                                *CLIENTE
021000021218     C                   MOVE(P)   W_SOG         ACOKSC
021100021218     C     KEYACO        CHAIN     CNACO00F
021200021218     C                   IF        not %found(CNACO00F)
021300021218     C                   MOVEL     '1'           ERRL00
021400021218     C                   EVAL      OG0ERR = 'E'
021500021218     C                   EVAL      OG0MSG = 'CLIENTE INESISTENTE'
021600021218     C                   ENDIF
021700021218     C                   ENDIF
021800021218     C*
021900021218     C                   IF        §GGBTIP = 'D'                                *DESTINATARIO
022000021218     C                   MOVE(P)   W_SOG         DSTCOD
022100021218     C     DSTCOD        CHAIN     AZDST04L
022200021218     C                   IF        not %found(AZDST04L)
022300021218     C                   MOVEL     '1'           ERRL00
022400021218     C                   EVAL      OG0ERR = 'E'
022500021218     C                   EVAL      OG0MSG = 'DESTINATARIO INESISTENTE'
022600021218     C                   ENDIF
022700021218     C                   ENDIF
022800021218     C*
022900021218     C                   IF        §GGBTIP = 'F'                                *P.O.
023000021218     C                   MOVE(P)   W_SOG         ORGFIL
023100021218     C     ORGFIL        CHAIN     AZORG01L
023200021218     C                   IF        not %found(AZORG01L)
023300021218     C                   MOVEL     '1'           ERRL00
023400021218     C                   EVAL      OG0ERR = 'E'
023500021218     C                   EVAL      OG0MSG = 'P.O. INESISTENTE'
023600021218     C                   ENDIF
023700021218     C                   ENDIF
023800021218     C                   ENDIF
023900021218     C*
024000021218     C* CONTROLLO ESISTENZA CODICE ARTICOLO
024100021218     C                   IF        ERRL00 = '0'
024200021218     C     IG0COD        CHAIN     AZAGB01L
024300021218     C                   IF        not %found(AZAGB01L)
024400021218     C                   MOVEL     '1'           ERRL00
024500021218     C                   EVAL      OG0ERR = 'E'
024600021218     C                   EVAL      OG0MSG = 'ARTICOLO INESISTENTE'
024700021218     C                   ENDIF
024800021218     C                   ENDIF
024900021218     C*
025000021230     C* CONTROLLO CHE I SOGGETTI DEI DUE MOVIMENTI CORRELATI NN SIANO MAI UGUALI
025100021230     C                   IF        ERRL00 = '0'
025200021230     C                   IF        IG0SOG <> IG0SMC
025300021230     C                   ELSE
025400021230     C                   MOVEL     '1'           ERRL00
025500021230     C                   EVAL      OG0ERR = 'E'
025600021230     C                   EVAL      OG0MSG = 'I SOGGETTI NON POSSONO COINCIDERE'
025700021230     C                   ENDIF
025800021230     C                   ENDIF
025900021218     C*
026000021218     C                   ENDSR
026100021218     C*------------------------------------------------------------------------*
026200021218     C* WRIMOV - SCRIVE I MOVIMENTI SUL FILE (AZMGB00F)
026300021218     C*------------------------------------------------------------------------*
026400021218     C     WRIMOV        BEGSR
026500021218     C*
026600021218     C* SOLO SE TUTTO OK PROCEDO CON LA SCRITTIRA EFFETTIVA DEI MOVIMENTI
026700021218     C                   IF        ERR_1GIRO = '0' AND
026800021218     C                             ERR_2GIRO = '0'
026900030122     C* REPERISCO UN NUMERATORE
027000150225     C*m                 EXSR      REPNUM
027100030122     C*
027200030122     C* SE ANCHE REPERIMENTO NUMERATORE E' OK => PROCEDO
027300150225     C*m                 IF        ERRNUM = '0'
027400021219     C                   CLEAR                   AZMGBXXX
027500021219     C                   EVAL      AZMGBXXX = AZMGB001
027600150227      *genera bolla se richiesta da causale
027700150227     c                   if        §ggbls01 ='S'
027800150227     c                   exsr      creabolla
027900150302     c                   if        §lpfpr = '3' or §lpfpr = '4'
028000150302     c                   eval      og0err = §lpfpr
028100150302     c                   eval      og0msg = 'Movimento non inserito bolla non +
028200150302     c                             confermata'
028300150302     c                   leavesr
028400150302     c                   endif
028500150302      *
028600150227     c                   eval      mgbrdo = %editc(§lplnp:'X') + '-'
028700150227     c                             + %editc(§lpnrs:'X') + '-'
028800150227     c                             + %editc(§lpnsp:'X')
028900150227     c                   movel     §lpaas        mgbddo
029000150227     c                   move      §lpmgs        mgbddo
029100150227     c                   endif
029200150225     c                   if        mgbtim <> 'M'
029300150330     c     kmgb5         setll     azmgb05l
029400150330     c                   do        *hival
029500150330     c     kmgb5         reade     azmgb05l
029600150330     c                   if        not %eof(azmgb05l)
029700150330     c                   if        w5mgbpou <> mgbpou
029800150330     c                   iter
029900150330     c                   endif
030000150302     c                   clear                   w5mgbrdo
030100150302     c                   clear                   w5mgbddo
030200150226     c                   add       mgbqta        w5mgbqta
030300150226     c                   move      mgbute        w5mgbute
030400150226     c                   move      mgbpou        w5mgbpou
030500150226     c                   z-add     mgbora        w5mgbora
030600150225     c                   update    azmgb5
030700150330     c                   leave
030800150225     c                   else
030900150225     c                   exsr      repnum
031000030122     C                   EVAL      mgbPRG = O33NRF
031100021218     C                   WRITE     AZMGB000
031200150330     c                   leave
031300150225     c                   endif
031400150330     c                   enddo
031500150225      *manuali sempre write
031600150225     c                   else
031700150225     c                   exsr      repnum
031800150225     C                   EVAL      mgbPRG = O33NRF
031900150225     C                   WRITE     AZMGB000
032000150225     c                   endif
032100021218     C* VERIFICO SE E' PREVISTO UN MOVIMENTO CORRELATO
032200021219     C                   IF        AZMGB002 > *blanks
032300021219     C                   CLEAR                   AZMGBXXX
032400021219     C                   EVAL      AZMGBXXX = AZMGB002
032500150225     c                   if        mgbtim <> 'M'
032600150330     c     kmgb5         setll     azmgb05l
032700150330     c                   do        *hival
032800150330     c     kmgb5         reade     azmgb05l
032900150330     c                   if        not %eof(azmgb05l)
033000150330     c                   if        w5mgbpou <> mgbpou
033100150330     c                   iter
033200150330     c                   endif
033300150226     c                   clear                   w5mgbrdo
033400150226     c                   clear                   w5mgbddo
033500150226     c                   add       mgbqta        w5mgbqta
033600150226     c                   move      mgbute        w5mgbute
033700150226     c                   move      mgbpou        w5mgbpou
033800150226     c                   z-add     mgbora        w5mgbora
033900150225     c                   update    azmgb5
034000150330     c                   leave
034100150225     c                   else
034200030122     C                   EVAL      mgbPRG = O33NRF
034300021218     C                   WRITE     AZMGB000
034400150330     c                   leave
034500150330     c                   endif
034600150330     c                   enddo
034700150225      *manuali sempre write
034800150225     c                   else
034900150225     C                   EVAL      mgbPRG = O33NRF
035000150225     C                   WRITE     AZMGB000
035100150225     c                   endif
035200021218     C                   ENDIF
035300150225     C*m                 ENDIF
035400030206     C                   ENDIF
035500150227     C                   ENDSR
035600150227      *____________________________________________________________
035700150227     c     creabolla     begsr
035800150227      *____________________________________________________________
035900150223      * Richiesta stampanti
036000150223     c                   reset                   TRUL90ds
036100150223     c                   call      'TRUL90R'
036200150223     c                   parm                    KPJBA
036300150223     c                   parm                    TRUL90ds
036400150223      *
036500150223     c                   if        D90F3   =  *on
036600150323     c                   eval      og0err = '3'
036700150223     c                   seton                                        lr
036800150223     c                   return
036900150223     c                   endif
037000150325      * OVRPRTF BOLLE A4
037100150325     C                   MOVEA     D90PRB4       CMA4f6(30)
037200150325     C                   MOVEA     D90MDB4       CMA4f6(52)
037300150325     C*
037400150325     C                   Z-ADD     130           LUNG             15 5
037500150325     C                   CLEAR                   COMMAN          130
037600150325     C                   MOVEA     CMA4f6        COMMAN
037700150325     C                   CALL      'QCMDEXC'                            91
037800150325     C                   PARM                    COMMAN
037900150325     C                   PARM                    LUNG
038000150325      *
038100150325     C                   if        not *in91
038200150325      *
038300150325      * OVRPRTF BOLLE A5
038400150325     C                   MOVEA     D90PRB5       CMA5f6(30)
038500150325     C                   MOVEA     D90MDB5       CMA5f6(52)
038600150325     C*
038700150325     C                   Z-ADD     130           LUNG
038800150325     C                   CLEAR                   COMMAN
038900150325     C                   MOVEA     CMA5f6        COMMAN
039000150325     C                   CALL      'QCMDEXC'                            91
039100150325     C                   PARM                    COMMAN
039200150325     C                   PARM                    LUNG
039300150325     c                   endif
039400150325     C                   if        not *in91
039500150325      *
039600150325      * OVRPRTF etichette
039700150325     C                   MOVEA     D90PRe        CMetf6(30)
039800150325     C                   MOVEA     D90MDE        CMetf6(52)
039900150325     C*
040000150325     C                   Z-ADD     130           LUNG
040100150325     C                   CLEAR                   COMMAN
040200150325     C                   MOVEA     CMetf6        COMMAN
040300150325     C                   CALL      'QCMDEXC'                            91
040400150325     C                   PARM                    COMMAN
040500150325     C                   PARM                    LUNG
040600150325     c                   endif
040700150223      *
040800150216     c                   exsr      preparaBLP
040900150205     c                   call      'FNLS01R'
041000150205     C                   PARM                    KPJBA
041100150205     C                   PARM                    DBLP
041200150205     C                   PARM                    DTASV
041300150205     C                   PARM                    trul90ds
041400021218     C*
041500021218     C                   ENDSR
041600150216     C*--------------------------------------------------------------------------------------------*
041700150216     C* preparaBLP- prepara i campi per richiamo inserimento bolle
041800150216     C*--------------------------------------------------------------------------------------------*
041900150216     C     preparaBLP    BEGSR
042000150220     c                   clear                   wlpnot           70
042100150220     c                   clear                   dtba
042200150223     C                   clear                   DBLP
042300150223     C                   clear                   DTASV
042400150223     C                   movel     acoksc        ORGFIL
042500150223     C     ORGFIL        CHAIN     AZORG01L
042600150220     c                   eval      tbeke1 = %subst(ig0cod:2:2)
042700150220     C                   EVAL      tbeCOD = 'TBA'
042800150220     c     ktbe          chain     tntbe01l
042900150220     c                   if        %found(tntbe01l)
043000150220     c                   movel     tbeuni        dtba
043100150220     c                   endif
043200150220     c                   eval      ktsc = %subst(ig0cod:1:1)
043300150220     c     kcgb          chain     azcgb01l
043400150223     c                   movel     datcor        §lpaas
043500150223     c                   move      datcor        §lpmgs
043600150226     c                   move      datcor        §lpdrt
043700150302     c                   move      'B'           §lpfpr
043800150216     c                   move      'A '          §lpcbo
043900150226     c                   move      '6 '          §lpctm
044000150226     c                   move      '§'           §lpsop
044100150226     c                   move      'M'           §lpfpp
044200150226     c                   z-add     1             §lprmn
044300150216     c                   z-add     8888888       §lpccm
044400150324     c                   movel     ig0pou        §lpccm
044500150216     c                   z-add     §lpccm        §lpccn
044600150226     c                   z-add     0000999       §lppdr
044700150324     c                   movel     ig0pou        §lppdr
044800150216     c                   movel     ORGDES        §LPRSM
044900150216     c                   movel     ORGIND        §LPINM
045000150216     c                   movel     ORGCPF        §LPCAM
045100150216     c                   movel     ORGLOC        §LPLOM
045200150216     c                   movel     ORGPRO        §LPPRM
045300150216      * Luogo invio fattura
045400150220     c                   move      IG0SOG        kspecli
045500150216     c     kFnspe        Chain     Fnspe01l
045600150216     c                   if        %found(fnspe01l) and speflg = *blank
045700150216     c                   movel     SPERAG        §LPRSd
045800150216     c                   movel     SPEIND        §LPINd
045900150216     c                   movel     SPECAP        §LPCAd
046000150216     c                   movel     SPELOC        §LPLOd
046100150216     c                   movel     SPEPRO        §LPPRd
046200150216     c                   movel     SPEnaz        §LPnzd
046300150216     c                   else
046400150220     c                   clear                   ds_cnaco
046500150220     c                   clear                   ds_cnind
046600150220     c                   clear                   ds_cnclp
046700150220     c                   clear                   ds_fncls
046800150220     c                   clear                   tibs69ds
046900150220     c                   move      ig0sog        i69kac
047000150223     c                   move      ig0sog        i69kin
047100150220     c                   call      'TIBS69R'
047200150220     c                   parm                    tibs69ds
047300150220     c                   parm                    ds_cnaco
047400150220     c                   parm                    ds_cnind
047500150220     c                   parm                    ds_cnclp
047600150220     c                   parm                    ds_fncls
047700150216     c                   movel     acorag        §LPRSd
047800150216     c                   movel     indvia        §LPINd
047900150216     c                   movel     indcap        §LPCAd
048000150216     c                   movel     indcit        §LPLOd
048100150216     c                   movel     indprv        §LPPRd
048200150216     c                   endif
048300150226     c                   eval      §lpnas = 'BANCALI'
048400150324     c                   eval      §lplpa = ig0pou
048500150324     c                   eval      §lplnp = ig0pou
048600150324     c                   eval      §lplna = ig0pou
048700150220     c                   eval      §lpncl = 1
048800150220     c                   z-add     ig0qta        wqta              5 0
048900150220     c                   eval      §lppkb = ig0qta * §tbapkg
049000150304     c                   if        cgbsco <> 0
049100150220     c                   eval      wlpnot = %trim ('RESO ' + %editc(wqta:'Z')
049200150223     c                             + ' ' + %trim(§tbadesc) + ' al netto del '+
049300150220     c                             %editc(cgbsco:'Z') + '% ' + 'di franchigia')
049400150304     c                   else
049500150304     c                   eval      wlpnot = %trim ('RESO ' + %editc(wqta:'Z')
049600150304     c                             + ' ' + %trim(§tbadesc) +' Senza franchigia')
049700150304     c                   endif
049800150220     c                   movel     wlpnot        §lpnot
049900150220     c                   move      wlpnot        §lpnt2
050000150216     C                   endsr
050100030122     C*--------------------------------------------------------------------------------------------*
050200030122     C* REPNUM - REPERISCE NUMERATORE
050300030122     C*--------------------------------------------------------------------------------------------*
050400030122     C     REPNUM        BEGSR
050500030122     C*
050600030122     C* INIALIAZIZZO FLAG ERRORE
050700030122     C                   MOVEL     '0'           ERRNUM
050800030122     C*
050900030122     C                   CLEAR                   TRUL33DS
051000030122     C                   EVAL      I33TLA = 'L'
051100030122     C                   EVAL      I33OPE = *zeros
051200030122     C                   EVAL      I33CNU = 031
051300030122     C                   EVAL      I33NUM = 1
051400030122     C                   MOVEL     TRUL33DS      KPJBU
051500030122     C                   CALL      'TRUL33R'
051600030122     C                   PARM                    KPJBA
051700030122     C                   MOVEL     KPJBU         TRUL33DS
051800030122     C                   IF        O33ERR <> *zeros
051900030122     C                   MOVEL     '1'           ERRNUM
052000030122     C                   MOVEL     O33MSG        OG0MSG
052100030122     C                   ENDIF
052200030122     C*
052300030122     C                   ENDSR
052400021218     C*--------------------------------------------------------------------------------------------*
052500021218     C* CARTAB - CARICA LE TABELLE OCCORRENTI
052600021218     C*--------------------------------------------------------------------------------------------*
052700021218     C     CARTAB        BEGSR
052800021218     C*
052900021218     C* NUOVA TABELLA CODICI CAUSALI MOVIMENTI GABBIE/BANCALI (GGB)
053000021230     C                   Z-ADD     *zeros        x
053100021218     C                   EVAL      tbeCOD = 'GGB'
053200021218     C     KEYTBE        SETLL     tntbe01l
053300021218     C     KEYTBE        READE     tntbe01l
053400021218     C                   DOW       not %eof(tntbe01l)
053500030122     C                   IF        tbeatb = *blanks
053600021218     C                   MOVEL     tbeuni        DGGB
053700021230     C                   ADD       1             x
053800021218     C                   MOVEL     tbeke1        KCAU(x)
053900021218     C                   MOVEL     DGGB          DCAU(x)
054000030122     C                   ENDIF
054100021218     C     KEYTBE        READE     tntbe01l
054200021218     C                   ENDDO
054300021218     C*
054400021218     C                   ENDSR
054500970526     C*------------------------------------------------------------------------*
054600970526     C* *INZSR - OPERAZIONI INIZIALI
054700970526     C*------------------------------------------------------------------------*
054800970526     C     *INZSR        BEGSR
054900970526     C*---
055000970526     C* RICEVIMENTO PARAMETRI
055100970526     C*---
055200970526     C     *ENTRY        PLIST
055300150227     C                   PARM                    kpjba
055400021218     C                   PARM                    TISIG0DS
055500021218     C*---
055600021218     C* INIZIALIZZAZIONE CAMPI DI WRK
055700021218     C*---
055800021218     C                   Z-ADD     1             ACOKUT
055900021218     C                   Z-ADD     151           ACOKCC
056000021218     C*---
056100021218     C* CHIAVI DI LETTURA
056200021218     C*---
056300021218     C* LETTURA TNTBE01L - PARZIALE
056400021218     C     KEYTBE        KLIST
056500021218     C                   KFLD                    tbeCOD                         *CODICE TABELLA
056600150220     C     Ktbe          KLIST
056700150220     C                   KFLD                    tbeCOD
056800150220     C                   KFLD                    tbeke1
056900150220     C     Kcgb          KLIST
057000150220     C                   KFLD                    ig0sog
057100150220     C                   KFLD                    ktsc              1
057200150225     C     Kmgb5         KLIST
057300150225     C                   KFLD                    MGBTIP
057400150225     C                   KFLD                    MGBSOG
057500150225     C                   KFLD                    MGBCOD
057600150225     C                   KFLD                    MGBCAU
057700150225     C                   KFLD                    MGBDAT
057800150216     C* LETTURA fnspe01l -
057900150216     c     kFnspe        Klist
058000150216     c                   Kfld                    kspefls           1
058100150216     c                   Kfld                    kspecli           7 0
058200150216     c                   Kfld                    kspecod           3
058300150216     c                   move      'L'           kspefls
058400150216     c                   move      '888'         kspecod
058500021218     C* LETTURA CNACO00F - COMPLETA
058600021218     C     KEYACO        KLIST
058700021218     C                   KFLD                    acoKUT                         *CODICE UTENTE
058800021218     C                   KFLD                    acoKCC                         *CODICE CAPOCONTO
058900021218     C                   KFLD                    acoKSC                         *CODICE SOTTOCONTO
059000021218     C*
059100021218     C* CARICA LE TABELLE OCCORRENTI
059200021218     C                   EXSR      CARTAB
059300020415     C*
059400020415     C* CALCOLA LA DATA CORRENTE
059500020415     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
059600020415     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
059700020415     C                   Z-ADD     WN8           G08DAT
059800020415     C                   Z-ADD     *ZEROS        G08INV
059900020415     C                   MOVEL     '0'           G08ERR
060000020415     C                   CALL      'XSRDA8'
060100020415     C                   PARM                    WLBDA8
060200020415     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
060300970526     C*
060400970526     C                   ENDSR
060500150325**  cmA4f6
060600150325OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
060700150325USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
060800150325**  cmA5f6
060900150325OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
061000150325             USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
061100150325**  cmETf6
061200150325OVRPRTF FILE(FNLV22P)   OUTQ(xxxxxxxxxx) FORMTYPE('XXXXXXXXXX')
061300150325USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
