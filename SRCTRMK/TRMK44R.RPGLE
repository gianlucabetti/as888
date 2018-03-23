000100950824     H DECEDIT('0,') DATEDIT(*DMY.)
000200950824     H* TRMK44R *-----------------------------------------------------*
000300950824     H*       CAMBIO CODICE COMMERCIALE (FILTRO)                      *
000400950824     H*---------------------------------------------------------------*
000500950824     FTRMK44D   CF   E             WORKSTN
000600100423     Fazorg01l  IF   E           K DISK
000700130730       // -?Anagrafica Commerciali?
000800130730     fAZCMM01L  if   e           k disk
000900130805
001000130805       // -?Costante per controllo "caratteri solo numerici"?
001100130805     d c_Digits        c                   const('0123456789')
001200041213
001300100922     d wabi            s                   like(§uteCLI)
001400041213
001500100423     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             Msg video
001600041213     d POG             S              3    DIM(250)
001700950825     D*
001800041213     d Azuteds       e ds                  extname(Azute00f)
001900041213     d dDatiute      e ds
002000041213     d dLat          e ds
002100041213     d dute01        e ds
002200041213     d Tibs02ds      e ds
002300130730     d TRMK43ds      e ds                  inz
002400041213     d Tibs34ds      e ds
002500041213     d Trul31ds      e ds                  INZ
002600100423     d og141         e ds                  INZ
002700041213
002800950825     D KPJBA         E DS
002900950825     D PARAM           DS
003000950825     D  V1CCMV                 1      7
003100950825     D  V1CCMN                 8     14
003200051216     d                sds
003300051216     d  V1Cpgm           *proc
003400100423     d
003500100423     d DataSYS         s               d   inz(*sys)
003600100423     d Dataiso         s               d   datfmt(*iso)
003700100423     d Alfaoggi        s              8
003800100423     d W0080           s              8  0
003900950831     C****************************************************************
004000950831     C*  RIEPILOGO INDICATORI
004100950831     C***************************************************************
004200041213      * 08    - qualsiasi tasto venga premuto va a fine pgm
004300950831     C* 32    - CODODO
004400950831     C* 40    - ERRORE CODICE COMMERCIALE VECCHIO
004500950831     C* 41    - ERRORE CODICE COMMERCIALE NUOVO
004600950904     C* 90    - ERRORE GENERICO
004700971203     C****************************************************************
004800950824     C*
004900950825     C* PULIZIA CAMPI VIDEO
005000950824     C                   CLEAR                   V1CCMV
005100950824     C                   CLEAR                   V1CCMN
005200950825     C                   CLEAR                   V1DCMV
005300950825     C                   CLEAR                   V1DCMN
005400950824     C*
005500950824     C     EMETTI        TAG
005600950824     C*
005700950824     C                   EXFMT     MK44D01
005800950904     C                   SETOFF                                       90
005900950824     C                   CLEAR                   V1ZMSG
006000950825     C* Premuto Tasto Fine => vado a fine
006100950824     C     *INKC         CABEQ     '1'           FINE
006200041213     C** 08 ON  - QUALSIASI TASTO VENGA PREMUTO, ESCO DAL PROGRAMMA
006300041213     C     *IN08         cabeq     *ON           fine
006400950824     C                   EXSR      CONTR
006500950825     C* Errore o non premuto tasto Conferma riemetto videata
006600950904     C     *IN90         IFEQ      *ON
006700950824     C     *INKF         OREQ      *OFF
006800950824     C                   GOTO      EMETTI
006900950824     C                   END
007000950825     C* Premuto tasto Conferma->lancio azione
007100950825     C                   MOVEL     PARAM         KPJBU
007200950825     C                   MOVEL     'MK45'        KCOAZ
007300950825     C                   CALL      'BCH10'
007400950825     C                   PARM                    KPJBA
007500950824     C*
007600950824     C*
007700950824     C     FINE          TAG
007800950824     C*
007900950824     C                   SETON                                        LR
008000950825     C*****************************************************************
008100950825     C**  CONTR - CONTROLLI VIDEATA                                  **
008200950825     C*****************************************************************
008300950825     C     CONTR         BEGSR
008400950825     C*
008500950825     C                   SETOFF                                       4041
008600950825     C                   CLEAR                   V1DCMV
008700950825     C                   CLEAR                   V1DCMN
008800950825     C*  CODICE COMMERCIALE DA SOSTITUIRE:
008900950825     C     '?'           SCAN      V1CCMV                                 32
009000950825     C*
009100950825    1C                   SELECT
009200950825     C*  Ricerca
009300950825     C     *IN32         WHENEQ    '1'
009400130730     c                   clear                   TRMK43ds
009500130730     c                   eval      iMK43fil = DUTpou
009600130730     c                   call      'TRMK43R'
009700130730     c                   parm                    KPJBA
009800130730     c                   parm                    TRMK43ds
009900130730     c                   if        oMK43err = *off  and
010000130730     c                             oMK43cmm > *zero
010100130730     C                   MOVEL     oMK43cmm      V1CCMV
010200130730     C                   MOVEL     oMK43des      V1DCMV
010300130730     c                   endif
010400950825     C*  Controlli:
010500950825     C     V1CCMV        WHENEQ    *BLANKS
010600971203     C                   MOVEL     MSG(1)        V1ZMSG
010700950904     C                   SETON                                        4090
010800971203     C**
010900950825     C     V1CCMV        WHENNE    *BLANKS
011000130805      *  Deve essere numerico
011100130805     c                   if        %check(c_Digits : V1Ccmv) > *zero
011200130805     c                   eval      V1Zmsg = Msg(2)
011300130805     c                   eval      *in40 = *on
011400130805     c                   eval      *in90 = *on
011500130805     c                   leavesr
011600130805     c                   endif
011700130730      *  Deve esistere in anagrafica
011800130730     c                   move      V1CcmV        CMMcod
011900130730     c     CMMcod        chain     AZCMM000
012000130730     c                   IF        Not %found(AZCMM01L)
012100110316     C                   MOVEL     MSG(2)        V1ZMSG
012200110316     C                   SETON                                        4090
012300110316     c                   leavesr
012400110316     c                   ENDIF
012500130730     c                   eval      v1dcmv = CMMdes
012600110316      * non deve avere particolarità
012700130730     c                   IF        CMMpar <> *blanks
012800110316     c                   eval      v1zmsg = msg(5)
012900110316     C                   SETON                                        4090
013000110316     c                   leavesr
013100110316     c                   ENDIF
013200950825     C*  Controllo i primi tre byte del codice: deve coincidere con una
013300950825     C*  filiale dell'area
013400041213     C                   MOVEL     V1CCMV        W0030             3
013500041213     C     W0030         LOOKUP    pog                                    32
013600971203    3C     *IN32         IFEQ      *OFF
013700041213     C     W0030         OREQ      *Zeros
013800041213     C     W0030         OREQ      *Blanks
013900971203     C                   MOVEL     MSG(4)        V1ZMSG
014000950904     C                   SETON                                        4090
014100971203    3C                   END
014200971203     C***
014300950825    1C                   ENDSL
014400950825     C*
014500971203    1C     *IN90         IFEQ      *OFF
014600950825     C*  CODICE COMMERCIALE NUOVO:
014700950825     C     '?'           SCAN      V1CCMN                                 32
014800950825     C*
014900971203    2C                   SELECT
015000950825     C*  Ricerca
015100950825     C     *IN32         WHENEQ    '1'
015200130730     c                   clear                   TRMK43ds
015300130730     c                   if        V1Ccmv > *zero
015400130730     c                   eval      iMK43fil = %int(V1CcmV) / 10000
015500130730     c                   else
015600130730     c                   eval      iMK43fil = DUTpou
015700130730     c                   endif
015800130730     c                   call      'TRMK43R'
015900130730     c                   parm                    KPJBA
016000130730     c                   parm                    TRMK43ds
016100130730     c                   if        oMK43err = *off  and
016200130730     c                             oMK43cmm > *zero
016300130730     C                   MOVEL     oMK43cmm      V1CCMN
016400130730     C                   MOVEL     oMK43des      V1DCMN
016500130730     c                   endif
016600950825     C*  Controlli:
016700950825     C     V1CCMN        WHENEQ    *BLANKS
016800971203     C                   MOVEL     MSG(1)        V1ZMSG
016900950904     C                   SETON                                        4190
017000971203     C**
017100950825     C     V1CCMN        WHENNE    *BLANKS
017200130730     C*  Deve esistere in anagrafica
017300130730     c                   move      V1CcmN        CMMcod
017400130730     c     CMMcod        chain     AZCMM000
017500130730     c                   IF        Not %found(AZCMM01L)
017600110316     C                   MOVEL     MSG(2)        V1ZMSG
017700110316     C                   SETON                                        4190
017800110316     c                   leavesr
017900110316    3c                   ENDIF
018000130730     c                   eval      v1dcmn = CMMdes
018100110316      * non deve avere particolarità
018200130730     c                   IF        CMMpar <> *blanks
018300110316     c                   eval      v1zmsg = msg(5)
018400110316     C                   SETON                                        4190
018500110316     c                   leavesr
018600110316     c                   ENDIF
018700111227      * deve essere un codice valido come data inizio e fine attività
018800111227      /free
018900130730         IF  CMMdtIni > w0080  or
019000130730             CMMdtFin < w0080;
019100111227           V1Zmsg = msg(5);
019200111227           *in41 = *on;
019300111227           *in90 = *on;
019400111227           leavesr;
019500111227         ENDIF;
019600111227      /end-free
019700051216      * Il nuovo codice commerciale deve essere dello stesso P.O. del
019800051216      *  vecchio codice commerciale
019900051216     c                   if        %subst(V1Ccmn:1:3) <>
020000051216     c                             %subst(V1Ccmv:1:3)
020100051216     c                   movel     Msg(7)        V1Zmsg
020200051216     c                   seton                                        4190
020300051216     c                   endif
020400971203     C**
020500971203    2C                   ENDSL
020600950825     C*
020700950825     C* I 2 CODICI COMMERCIALI DEVONO ESSERE DIVERSI
020800971203    2C     V1CCMV        IFEQ      V1CCMN
020900950904     C  N90              MOVEL     MSG(3)        V1ZMSG
021000950904     C                   SETON                                        404190
021100971203    2C                   END
021200971203    1C                   ENDIF
021300950825     C*
021400950825     C                   ENDSR
021500950825     C*****************************************************************
021600950825     C**  ROUTINE INIZIALE                                           **
021700950825     C*****************************************************************
021800950825     C     *INZSR        BEGSR
021900950825     C*
022000950825     C     *ENTRY        PLIST
022100950825     C                   PARM                    KPJBA
022200950825     C*
022300041213     c     *dtaara       define    §azute        azuteds
022400041213     c     *dtaara       define    §datiute      ddatiute
022500041213     c                   in(E)     *dtaara
022600041213     c                   If        %error  or RSUT = *blanks
022700041213     c                   Clear                   Tibs34ds
022800041213     c                   Call      'TIBS34R'
022900041213     c                   Parm                    Tibs34ds
023000041213     c                   In        *dtaara
023100041213     c                   EndIf
023200041213
023300100922     c                   clear                   Dute01
023400100922if  1c                   if        DutErr <> 'E'
023500100920     c                   Movel     UteFaf        Dute01
023600100922e   1c                   endif
023700100920      *
023800041213     c                   Clear                   wabi
023900041213     c                   Clear                   dLat
024000041213
024100041213      * Verifica errori e autorità profilo
024200041213s   1c                   Select
024300041213      * se ho errori nei dati utente esco dal pgm
024400041213w   1c                   When      DutErr = 'E'
024500041213     c                   GoTo      Fine
024600100922      * se sono utente di sede non posso lanciare questo programma
024700100922      * perchè non ci sono tutti i dati in linea....è da lanciare in ambiente di
024800100922      * filiale
024900100922w   1c                   When      DutPou = 046   or
025000100922     c                             DutLpo = 'S'
025100100922     c                   Seton                                        409008
025200100922     C                   movel     msg(08)       v1zmsg
025300100922     c                   Leavesr
025400100920      * se utente NON autorizzato a questa funzione specifica
025500100920      *   esce dal *pgm
025600100920w   1c                   When      §UteChgCmm <> 'S'
025700100920     c                   Seton                                          9008
025800100920     C                   movel     msg(10)       v1zmsg
025900100920     c                   leavesr
026000041213      * se non c'è l'abilitazione
026100041213      * --> se 1° livello, abilitazioni al terminal
026200041213      *     se 2° livello, abilitazioni al punto operativo
026300100922w   1c                   When      §UteCLI = *Blanks
026400041213if  2c                   If        DutLpo = '1'
026500041213     c                   Eval      wabi   = 'TP'
026600041213e   2c                   EndIf
026700041213if  2c                   If        DutLpo = '2'
026800041213     c                   Eval      wabi   = 'PO'
026900041213e   2c                   EndIf
027000041213      * carica le abilitazioni del profilo
027100041213x   1c                   Other
027200100922     c                   Eval      wabi = §UteCLI
027300041213e   1c                   EndSl
027400041213
027500041213      * controllo se ok l'abilitazione dell'utente
027600041213     c                   Clear                   Tibs02ds
027700041213     c                   Eval      T02Mod = 'C'
027800041213     c                   Eval      T02Sif = knsif
027900041213     c                   Eval      T02Cod = 'LAT'
028000041213     c                   Movel(p)  wabi          T02Ke1
028100041213     c                   Call      'TIBS02R'
028200041213     c                   Parm                    kpjba
028300041213     c                   Parm                    Tibs02ds
028400041213if  1c                   If        T02Err = *Blanks
028500041213     c                   Eval      dLat = T02Uni
028600041213e   1c                   EndIf
028700041213      * errore o non abilitato
028800041213if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
028900041213     c                   Seton                                        409008
029000041213     C                   movel     msg(06)       v1zmsg
029100041213e   1c                   EndIf
029200041213      * Reperimento dei P.O. gestibili dall'utente
029300041213     c                   clear                   TRUL31DS
029400041213     c                   eval      I31abi = wabi
029500041213     c                   eval      I31cdi = DUTdis
029600041213     c                   eval      I31car = DUTare
029700041213     c                   eval      I31cpo = DUTpou
029800041213     c                   call      'TRUL31R'
029900041213     c                   parm                    KPJBA
030000041213     c                   parm                    TRUL31DS
030100041213if  1c                   if        O31pog > *zeros
030200041213     c                   movea     O31pog        pog
030300041213x   1c                   else
030400041213     c                   Seton                                        409008
030500041213     c                   movel     MSG(06)       v1zmsg
030600041213e   1c                   endif
030700950825     C*
030800100423     c* data del giorno
030900100423     c                   eval      dataiso=datasys
031000100423     c                   eval      w0080=%dec(dataiso)
031100100423     c                   eval      Alfaoggi=%editc(w0080:'X')
031200100423     c
031300100423     c* Verifico se partito con new gestione trattative --> non utilizzabile
031400100423     c                   if        not *in08
031500130730      *
031600100423     c     dutpou        chain     azorg01l
031700100423     c                   if        %found(azorg01l)
031800100423     c                   eval      og141=orgde1
031900100423     c                   else
032000100423     c                   clear                   og141
032100100423     c                   endif
032200130730      *
032300100423     c                   endif
032400950825     C*
032500950825     C                   ENDSR
032600981204**  MSG
032700950825Immettere un Codice Commerciale
032800950825Codice Commerciale inesistente
032900950825I codici commerciali NON devono essere uguali
033000981204Codice commerciale di altra area
033100981204Codice commerciale non utilizzabile
033200041213Utente non abilitato alla gestione dei potenziali!!!
033300051216Codice commerciale di altro P.O.
033400060124Questa funzione deve essere lanciata in ambiente di Filiale!!!!
033500100423Funzione non più abilitata per utente di filiale con NUOVA GESTIONE TRATTATIVE 9
033600100920Utente non abilitato al cambio del codice commerciale
