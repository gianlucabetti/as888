000100960530     H DECEDIT('0,') DATEDIT(*DMY/)
000200050901     F*                                                                     *
000300050901     C*? PARAMETRI:
000400050901     F*       obbligatorio INPUT-->  sndrcv   = S(snd) o R(rcv)             *
000500050901     F*           Snd Rcv--Out       tiporcv  = identifica SUM/STA          *
000600050901     F*               Snd--Out       clisnd   = codice cliente              *
000700050901     F*               Snd--Out       sigsnd   = sigla cliente               *
000800050901     C*?
000900050901     F*---------------------------------------------------------------------*
001000050831     Fwfrcv00w  if   e             disk    usropn
001100050831     Fwfsnd00w  if   e             disk    usropn
001200050831     fedtab01l  if   e           k disk
001300960530     D*---------------------------------------------------------------------*
001400050831     D EDTT00DS      E DS
001500050829     D WDAT            DS          1950
001600050829     D  SUMPRG                 1      4  0
001700050829     D  SUMTPR                 5      8
001800050831
001900050831      *parametri
002000050831     D  sndrcv         s              1
002100050831     D  tiporcv        s              4
002200050831     D  clisnd         s              7
002300050831     D  sigsnd         s              2
002400050831     D CPT             S             35    DIM(100)
002500050831     D CPTparz         S             35    DIM(100)
002600050831     D DPT             S             90    DIM(100)
002700050831     D DPU             S             90    DIM(100)
002800050831     D EDIDSPT       E DS
002900050831     D EDIDSPU       E DS
003000050831     D W035A           s             35
003100101117      *
003200101117      *  Invio E-mail
003300101117     D Indirizzi       s            253
003400101117     D Oggetto         s             44
003500101117     D Messaggio       s           5000
003600101117      *
003700101117      *---------------------------------------------------------------------------
003800101117     d bart_it         C                   CONST('@Bartolini.it;')
003900101117     d CED_Bart        C                   CONST('CED@Bartolini.it;')
004000101117     d CED_Alert       C                   CONST('CedAlert@Bartolini.it;')
004100101117     d AB_mail         C                   CONST('Andrea.Bertocchi')
004200050831      *---------------------------------------------------------------------------
004300050831     C*? Seleziona il Flat File
004400050831     c                   if        sndrcv = 'S'
004500050901     c                   open      wfsnd00w
004600050831     c                   exsr      filesnd
004700050831     c                   else
004800050901     c                   open      wfrcv00w
004900050831     c                   exsr      filercv
005000050831     c                   endif
005100050831
005200050831     C                   SETON                                        LR
005300050831     C*------------------------------------------------------------
005400050831     c     filesnd       begsr
005500050831     C*------------------------------------------------------------
005600050831     C                   READ      wfsnd00w
005700050831     C                   If        not %EOF(wfsnd00w)
005800050901     c                   movel     flatrec       wdat
005900050831     c                   movel     flatrec       edtt00ds
006000050831     c                   exsr      repcli
006100050831     c                   endif
006200050901     c                   move      sumtpr        tiporcv
006300050831     c                   endsr
006400050831     C*
006500050831     C*------------------------------------------------------------
006600050831     c     filercv       begsr
006700050831     C*------------------------------------------------------------
006800050829     C                   READ      wfrcv00w
006900101117      *
007000050829     C                   If        not %EOF(wfrcv00w)
007100050829     c                   movel     flatrec       wdat
007200050829     c                   endif
007300101117      *
007400050901     c                   move      sumtpr        tiporcv
007500101117      *
007600101117      *  se il tipo non è previsto invia mail a CEDALERT di avviso sul WFRCV00W
007700101117      *  PER INTERVENIRE ed eliminare le righe deletandole fisicamente x poter
007800101117      *  far ricevere i messaggi EDI.
007900101117     c                   if        tiporcv <> 'TT00' and
008000101117     c                             tiporcv <> 'ST00'
008100101117     c                   exsr      Invio_Mail
008200101117     c                   end
008300101117      *
008400050831     c                   endsr
008500050831     C*------------------------------------------------------*
008600050831      * Cerca il codice del Cliente/Partner da Tab PT
008700050831     C*------------------------------------------------------*
008800050831     C     repcli        BEGSR
008900050831      *
009000050831      * Prova con il codice + qualificatore
009100050831     C                   MOVEL     TA0010        WCOD             35
009200050831     C                   MOVE      TAA007        WCOD
009300050831     C                   Z-ADD     1             XX                3 0
009400050831     C     WCOD          LOOKUP    CPT(XX)                                32
009500050831      *
009600050831      * se non l'ha trovato prova parzialmente senza il qualificatore
009700050831     C                   If        *IN32 = *off
009800050831     C                   MOVEL     ta0010        WCOD
009900050831     C                   eval      XX = 1
010000050831     C                   clear                   W035A
010100050831     C                   eval      W035A = %subst(WCod:1:31)
010200050831     C     W035A         lookup    CPTparz(XX)                            32
010300050831     C                   Endif
010400050831      *
010500050831      * Se ha trovato la tabella prende il codice del cliente e lo passa
010600050831      *  nel campo generico di INET
010700050831     C                   If        *IN32 = *on
010800050831     c                   movel     DPT(XX)       edidspt
010900050831     c                   movel     DPu(XX)       edidspu
011000050831     c                   move      *zeros        Clisnd
011100050831     c                   move      §PtKSC        Clisnd
011200050831     c                   move      §Pusig        sigsnd
011300050831     C                   Endif
011400050831      *
011500050831     C                   ENDSR
011600960530     C*
011700960530     C*----------------------------------------------------------------
011800960530     C*  *INZSR - OPERAZIONI INIZIALI
011900960530     C*----------------------------------------------------------------
012000960530     C     *INZSR        BEGSR
012100960530     C*
012200960530     C     *ENTRY        PLIST
012300050831     C                   PARM                    sndrcv
012400050831     C                   PARM                    tiporcv
012500050901     C                   PARM                    clisnd
012600050901     C                   PARM                    sigsnd
012700050901     c                   clear                   tiporcv
012800050901     c                   clear                   clisnd
012900050901     c                   clear                   sigsnd
013000050831      * ----------------
013100050831      * Caricamento Tabella Partner esteri
013200050831     C                   Z-ADD     0             X                 3 0
013300050831     C                   MOVEL     'PT'          TABCOD
013400050831     C     TABCOD        CHAIN     EDTAB01L                           30
013500050831     C     *IN30         DOWEQ     '0'
013600050831     C     TABFLG        ifeq      *BLANKS
013700050831     C                   ADD       1             X
013800050831     C                   MOVEL     TABKEY        CPT(X)
013900050831     C                   eval      CPTparz(X) = %subst(TABKEY:1:30)
014000050831     C                   MOVEL     TABUNI        DPT(X)
014100050831     C                   end
014200050831     C     TABCOD        READE     EDTAB01L                               30
014300050831     C                   END
014400050901      * tab/PU
014500050901     C                   Z-ADD     0             X                 3 0
014600050901     C                   MOVEL     'PU'          TABCOD
014700050901     C     TABCOD        CHAIN     EDTAB01L                           30
014800050901     C     *IN30         DOWEQ     '0'
014900050901     C     TABFLG        ifeq      *BLANKS
015000050901     C                   ADD       1             X
015100050901     C                   MOVEL     TABUNI        DPu(X)
015200050901     C                   end
015300050901     C     TABCOD        READE     EDTAB01L                               30
015400050901     C                   END
015500050901      *
015600960530     C                   ENDSR
015700101117     c*==================================================================*
015800101117      * Manda un Msg x E-mail
015900101117     c*==================================================================*
016000101117     c     Invio_Mail    begsr
016100101117      *
016200101117     c                   eval      indirizzi = CED_Alert
016300101117     C                   eval      wrkEml = Indirizzi
016400101117     c                   eval      Oggetto ='Problemi File WFRCV00W ricez.EDI: -
016500101117     c                             su 1°Rcd'
016600101117     C                   eval      Messaggio = 'Nel File WFRCV00W ricezione EDI-
016700101117     c                              sono presenti records NON riconosciuti per -
016800101117     c                             la traduzione. Se non vengono eliminati, tut-
016900101117     c                             ti i msg EDI da portare su AS andranno in er-
017000101117     c                             rore. Eliminare le righe NON appartenenti a -
017100101117     c                             EDI.'
017200101117      *
017300101117     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
017400101117     C* Inizializzo variabili
017500101117     C                   movel     *blanks       wrkEml          253
017600101117     C                   movel     *blanks       wrkMsg         5000
017700101117     C                   movel     *blanks       wrkOgg           44
017800101117      *
017900101117     C* Valorizzo i campi della e-m@ail - indirizzo
018000101117     C                   eval      wrkEml = Indirizzi
018100101117     C                   eval      wrkOgg = Oggetto
018200101117     C                   eval      wrkMsg = Messaggio
018300110203      ********
018400110203     C********           call(e)   'TIS701C'
018500110203     C********           parm                    wrkEml
018600110203     C********           parm                    wrkOgg
018700110203     C********           parm                    wrkMsg
018800110203     C*
018900110203      ** *****
019000110203     C                   call(e)   'TRTCT00R2'
019100110203     C                   parm                    wrkEml
019200110203     C                   parm                    wrkOgg
019300110203     C                   parm                    wrkMsg
019400110203     C*
019500101117     C*
019600101117     C                   ENDSR
019700101117      *
019800101117     ***********************************************************************
