000100950824     H DECEDIT('0,') DATEDIT(*DMY.)
000200950824     H* TRMK44R *-----------------------------------------------------*
000300950824     H*       CAMBIO CODICE COMMERCIALE (FILTRO)                      *
000400950824     H*---------------------------------------------------------------*
000500950824     FTRMK44D   CF   E             WORKSTN
000600950824     FTABEL00F  IF   E           K DISK
000700100423     Fazorg01l  IF   E           K DISK
000800041213
000900041213     d codut           s              1  0 Inz(1)
001000100922     d wabi            s                   like(§uteCLI)
001100041213
001200100423     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             Msg video
001300041213     d POG             S              3    DIM(250)
001400950825     D*
001500041213     d Azuteds       e ds                  extname(Azute00f)
001600041213     d dDatiute      e ds
001700041213     d dLat          e ds
001800041213     d dute01        e ds
001900041213     d Tibs02ds      e ds
002000041213     d Tibs34ds      e ds
002100041213     d Trul31ds      e ds                  INZ
002200100423     d og141         e ds                  INZ
002300110316
002400110316     d ds01          e ds
002500041213
002600950825     D KPJBA         E DS
002700950825     D PARAM           DS
002800950825     D  V1CCMV                 1      7
002900950825     D  V1CCMN                 8     14
003000051216     d                sds
003100051216     d  V1Cpgm           *proc
003200100423     d
003300100423     d DataSYS         s               d   inz(*sys)
003400100423     d Dataiso         s               d   datfmt(*iso)
003500100423     d Alfaoggi        s              8
003600100423     d W0080           s              8  0
003700950831     C****************************************************************
003800950831     C*  RIEPILOGO INDICATORI
003900950831     C***************************************************************
004000041213      * 08    - qualsiasi tasto venga premuto va a fine pgm
004100950831     C* 32    - CODODO
004200950831     C* 40    - ERRORE CODICE COMMERCIALE VECCHIO
004300950831     C* 41    - ERRORE CODICE COMMERCIALE NUOVO
004400950904     C* 90    - ERRORE GENERICO
004500971203     C****************************************************************
004600950824     C*
004700950825     C* PULIZIA CAMPI VIDEO
004800950824     C                   CLEAR                   V1CCMV
004900950824     C                   CLEAR                   V1CCMN
005000950825     C                   CLEAR                   V1DCMV
005100950825     C                   CLEAR                   V1DCMN
005200950824     C*
005300950824     C     EMETTI        TAG
005400950824     C*
005500950824     C                   EXFMT     MK44D01
005600950904     C                   SETOFF                                       90
005700950824     C                   CLEAR                   V1ZMSG
005800950825     C* Premuto Tasto Fine => vado a fine
005900950824     C     *INKC         CABEQ     '1'           FINE
006000041213     C** 08 ON  - QUALSIASI TASTO VENGA PREMUTO, ESCO DAL PROGRAMMA
006100041213     C     *IN08         cabeq     *ON           fine
006200950824     C                   EXSR      CONTR
006300950825     C* Errore o non premuto tasto Conferma riemetto videata
006400950904     C     *IN90         IFEQ      *ON
006500950824     C     *INKF         OREQ      *OFF
006600950824     C                   GOTO      EMETTI
006700950824     C                   END
006800950825     C* Premuto tasto Conferma->lancio azione
006900950825     C                   MOVEL     PARAM         KPJBU
007000950825     C                   MOVEL     'MK45'        KCOAZ
007100950825     C                   CALL      'BCH10'
007200950825     C                   PARM                    KPJBA
007300950824     C*
007400950824     C*
007500950824     C     FINE          TAG
007600950824     C*
007700950824     C                   SETON                                        LR
007800950825     C*****************************************************************
007900950825     C**  CONTR - CONTROLLI VIDEATA                                  **
008000950825     C*****************************************************************
008100950825     C     CONTR         BEGSR
008200950825     C*
008300950825     C                   SETOFF                                       4041
008400950825     C                   CLEAR                   V1DCMV
008500950825     C                   CLEAR                   V1DCMN
008600950825     C*  CODICE COMMERCIALE DA SOSTITUIRE:
008700950825     C     '?'           SCAN      V1CCMV                                 32
008800950825     C*
008900950825    1C                   SELECT
009000950825     C*  Ricerca
009100950825     C     *IN32         WHENEQ    '1'
009200950825     C                   MOVEL     '01'          §COD
009300010620     C                   CLEAR                   §KEY
009400010620     C                   CALL      'TRTB85R'
009500010620     C                   PARM                    KPJBA
009600010620     C                   PARM                    §FIL              3
009700010620     C                   PARM                    §KEY
009800010620     C                   PARM                    §DES1            25
009900950825     C                   MOVEL     §KEY          V1CCMV
010000010620     C                   MOVEL     §DES1         V1DCMV
010100950825     C*  Controlli:
010200950825     C     V1CCMV        WHENEQ    *BLANKS
010300971203     C                   MOVEL     MSG(1)        V1ZMSG
010400950904     C                   SETON                                        4090
010500971203     C**
010600950825     C     V1CCMV        WHENNE    *BLANKS
010700950825     C*  Deve esistere in tabella
010800110316     c                   clear                   ds01
010900950825     C                   MOVE      '01'          KCOD
011000950825     C                   MOVEL(P)  V1CCMV        KKEY
011100950825     C     KTAB          CHAIN     TABEL                              32
011200110316     c                   IF        *in32
011300110316     C                   MOVEL     MSG(2)        V1ZMSG
011400110316     C                   SETON                                        4090
011500110316     c                   leavesr
011600110316     c                   ENDIF
011700110316     c                   eval      ds01 = tbluni
011800110316     c                   eval      v1dcmv = §01age
011900110316      * non deve avere particolarità
012000110316     c                   IF        §01par <> *blanks
012100110316     c                   eval      v1zmsg = msg(5)
012200110316     C                   SETON                                        4090
012300110316     c                   leavesr
012400110316     c                   ENDIF
012500950825     C*  Controllo i primi tre byte del codice: deve coincidere con una
012600950825     C*  filiale dell'area
012700041213     C                   MOVEL     V1CCMV        W0030             3
012800041213     C     W0030         LOOKUP    pog                                    32
012900971203    3C     *IN32         IFEQ      *OFF
013000041213     C     W0030         OREQ      *Zeros
013100041213     C     W0030         OREQ      *Blanks
013200971203     C                   MOVEL     MSG(4)        V1ZMSG
013300950904     C                   SETON                                        4090
013400971203    3C                   END
013500971203     C***
013600950825    1C                   ENDSL
013700950825     C*
013800971203    1C     *IN90         IFEQ      *OFF
013900950825     C*  CODICE COMMERCIALE NUOVO:
014000950825     C     '?'           SCAN      V1CCMN                                 32
014100950825     C*
014200971203    2C                   SELECT
014300950825     C*  Ricerca
014400950825     C     *IN32         WHENEQ    '1'
014500950825     C                   MOVEL     '01'          §COD
014600010621     C                   CLEAR                   §KEY
014700010621     C                   CALL      'TRTB85R'
014800010621     C                   PARM                    KPJBA
014900010621     C                   PARM                    §FIL              3
015000010621     C                   PARM                    §KEY
015100010621     C                   PARM                    §DES1            25
015200950825     C                   MOVEL     §KEY          V1CCMN
015300010621     C                   MOVEL     §DES1         V1DCMN
015400950825     C*  Controlli:
015500950825     C     V1CCMN        WHENEQ    *BLANKS
015600971203     C                   MOVEL     MSG(1)        V1ZMSG
015700950904     C                   SETON                                        4190
015800971203     C**
015900950825     C     V1CCMN        WHENNE    *BLANKS
016000950825     C*  Deve esistere in tabella
016100110316     c                   clear                   ds01
016200950825     C                   MOVE      '01'          KCOD
016300950825     C                   MOVEL(P)  V1CCMN        KKEY
016400950825     C     KTAB          CHAIN     TABEL                              32
016500110316    3c                   IF        *in32
016600110316     C                   MOVEL     MSG(2)        V1ZMSG
016700110316     C                   SETON                                        4190
016800110316     c                   leavesr
016900110316    3c                   ENDIF
017000110316     c                   eval      ds01 = tbluni
017100110316     c                   eval      v1dcmn = §01age
017200110316      * non deve avere particolarità
017300110316     c                   IF        §01par <> *blanks
017400110316     c                   eval      v1zmsg = msg(5)
017500110316     C                   SETON                                        4190
017600110316     c                   leavesr
017700110316     c                   ENDIF
017800111227      * deve essere un codice valido come data inizio e fine attività
017900111227      /free
018000111227         IF  §01dtdec > %editc(w0080:'X') or
018100111227             §01dtfin < %editc(w0080:'X');
018200111227           V1Zmsg = msg(5);
018300111227           *in41 = *on;
018400111227           *in90 = *on;
018500111227           leavesr;
018600111227         ENDIF;
018700111227      /end-free
018800051216      * Il nuovo codice commerciale deve essere dello stesso P.O. del
018900051216      *  vecchio codice commerciale
019000051216     c                   if        %subst(V1Ccmn:1:3) <>
019100051216     c                             %subst(V1Ccmv:1:3)
019200051216     c                   movel     Msg(7)        V1Zmsg
019300051216     c                   seton                                        4190
019400051216     c                   endif
019500971203     C**
019600971203    2C                   ENDSL
019700950825     C*
019800950825     C* I 2 CODICI COMMERCIALI DEVONO ESSERE DIVERSI
019900971203    2C     V1CCMV        IFEQ      V1CCMN
020000950904     C  N90              MOVEL     MSG(3)        V1ZMSG
020100950904     C                   SETON                                        404190
020200971203    2C                   END
020300971203    1C                   ENDIF
020400950825     C*
020500950825     C                   ENDSR
020600950825      *****************************************************************---------
020700950825      * RICERCA ELEMENTI DI TABELLA
020800950825      *****************************************************************---------
020900950825     C     FINTAB        BEGSR                                                  *** FINTAB ***
021000950825     C                   MOVEL     CODUT         §KUT
021100981204     C                   CLEAR                   §KEY
021200950825     C                   CALL      'X§TABER'
021300950825     C                   PARM                    §KUT
021400950825     C                   PARM                    §COD
021500950825     C                   PARM                    §KEY
021600950825     C                   PARM                    §DES             30
021700950825     C                   ENDSR
021800950825     C*****************************************************************
021900950825     C**  ROUTINE INIZIALE                                           **
022000950825     C*****************************************************************
022100950825     C     *INZSR        BEGSR
022200950825     C*
022300950825     C     *ENTRY        PLIST
022400950825     C                   PARM                    KPJBA
022500950825     C*
022600950825     C     KTAB          KLIST
022700950825     C                   KFLD                    CODUT
022800950825     C                   KFLD                    KCOD              2
022900950825     C                   KFLD                    KKEY              8
023000950825     C*
023100041213     c     *dtaara       define    §azute        azuteds
023200041213     c     *dtaara       define    §datiute      ddatiute
023300041213     c                   in(E)     *dtaara
023400041213     c                   If        %error  or RSUT = *blanks
023500041213     c                   Clear                   Tibs34ds
023600041213     c                   Call      'TIBS34R'
023700041213     c                   Parm                    Tibs34ds
023800041213     c                   In        *dtaara
023900041213     c                   EndIf
024000041213
024100100922     c                   clear                   Dute01
024200100922if  1c                   if        DutErr <> 'E'
024300100920     c                   Movel     UteFaf        Dute01
024400100922e   1c                   endif
024500100920      *
024600041213     c                   Clear                   wabi
024700041213     c                   Clear                   dLat
024800041213
024900041213      * Verifica errori e autorità profilo
025000041213s   1c                   Select
025100041213      * se ho errori nei dati utente esco dal pgm
025200041213w   1c                   When      DutErr = 'E'
025300041213     c                   GoTo      Fine
025400100922      * se sono utente di sede non posso lanciare questo programma
025500100922      * perchè non ci sono tutti i dati in linea....è da lanciare in ambiente di
025600100922      * filiale
025700100922w   1c                   When      DutPou = 046   or
025800100922     c                             DutLpo = 'S'
025900100922     c                   Seton                                        409008
026000100922     C                   movel     msg(08)       v1zmsg
026100100922     c                   Leavesr
026200100920      * se utente NON autorizzato a questa funzione specifica
026300100920      *   esce dal *pgm
026400100920w   1c                   When      §UteChgCmm <> 'S'
026500100920     c                   Seton                                          9008
026600100920     C                   movel     msg(10)       v1zmsg
026700100920     c                   leavesr
026800041213      * se non c'è l'abilitazione
026900041213      * --> se 1° livello, abilitazioni al terminal
027000041213      *     se 2° livello, abilitazioni al punto operativo
027100100922w   1c                   When      §UteCLI = *Blanks
027200041213if  2c                   If        DutLpo = '1'
027300041213     c                   Eval      wabi   = 'TP'
027400041213e   2c                   EndIf
027500041213if  2c                   If        DutLpo = '2'
027600041213     c                   Eval      wabi   = 'PO'
027700041213e   2c                   EndIf
027800041213      * carica le abilitazioni del profilo
027900041213x   1c                   Other
028000100922     c                   Eval      wabi = §UteCLI
028100041213e   1c                   EndSl
028200041213
028300041213      * controllo se ok l'abilitazione dell'utente
028400041213     c                   Clear                   Tibs02ds
028500041213     c                   Eval      T02Mod = 'C'
028600041213     c                   Eval      T02Sif = knsif
028700041213     c                   Eval      T02Cod = 'LAT'
028800041213     c                   Movel(p)  wabi          T02Ke1
028900041213     c                   Call      'TIBS02R'
029000041213     c                   Parm                    kpjba
029100041213     c                   Parm                    Tibs02ds
029200041213if  1c                   If        T02Err = *Blanks
029300041213     c                   Eval      dLat = T02Uni
029400041213e   1c                   EndIf
029500041213      * errore o non abilitato
029600041213if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
029700041213     c                   Seton                                        409008
029800041213     C                   movel     msg(06)       v1zmsg
029900041213e   1c                   EndIf
030000041213      * Reperimento dei P.O. gestibili dall'utente
030100041213     c                   clear                   TRUL31DS
030200041213     c                   eval      I31abi = wabi
030300041213     c                   eval      I31cdi = DUTdis
030400041213     c                   eval      I31car = DUTare
030500041213     c                   eval      I31cpo = DUTpou
030600041213     c                   call      'TRUL31R'
030700041213     c                   parm                    KPJBA
030800041213     c                   parm                    TRUL31DS
030900041213if  1c                   if        O31pog > *zeros
031000041213     c                   movea     O31pog        pog
031100041213x   1c                   else
031200041213     c                   Seton                                        409008
031300041213     c                   movel     MSG(06)       v1zmsg
031400041213e   1c                   endif
031500950825     C*
031600100423     c* data del giorno
031700100423     c                   eval      dataiso=datasys
031800100423     c                   eval      w0080=%dec(dataiso)
031900100423     c                   eval      Alfaoggi=%editc(w0080:'X')
032000100423     c
032100100423     c* Verifico se partito con new gestione trattative --> non utilizzabile
032200100423     c                   if        not *in08
032300100423     c
032400100423     c     dutpou        chain     azorg01l
032500100423     c                   if        %found(azorg01l)
032600100423     c                   eval      og141=orgde1
032700100423     c                   else
032800100423     c                   clear                   og141
032900100423     c                   endif
033000100423
033100100519     c**                 if        §ogigestrt>*zeros  and §ogigestrt<=alfaoggi
033200100519     c**                 seton                                        289008
033300100519     c**                 movel     msg(09)       v1zmsg
033400100519     c**                 endif
033500100423     c                   endif
033600100423     C*
033700950825     C     *LIKE         DEFINE    TBLKUT        §KUT
033800950825     C     *LIKE         DEFINE    TBLCOD        §COD
033900950825     C     *LIKE         DEFINE    TBLKEY        §KEY
034000950825     C*
034100950825     C                   ENDSR
034200981204**  MSG
034300950825Immettere un Codice Commerciale
034400950825Codice Commerciale inesistente
034500950825I codici commerciali NON devono essere uguali
034600981204Codice commerciale di altra area
034700981204Codice commerciale non utilizzabile
034800041213Utente non abilitato alla gestione dei potenziali!!!
034900051216Codice commerciale di altro P.O.
035000060124Questa funzione deve essere lanciata in ambiente di Filiale!!!!
035100100423Funzione non più abilitata per utente di filiale con NUOVA GESTIONE TRATTATIVE 9
035200100920Utente non abilitato al cambio del codice commerciale
