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
002400121105     D CPT             S             35    DIM(200)
002500121105     D CPTparz         S             35    DIM(%elem(CPT))
002600121105     D DPT             S             90    DIM(%elem(CPT))
002700121105     D DPU             S             90    DIM(%elem(CPT))
002800121105     D TRUL0SDS      E DS
002900121105     D KPJBA         E DS
003000121112     d kpjbusav        S                   like(Kpjbu)
003100050831     D EDIDSPT       E DS
003200050831     D EDIDSPU       E DS
003300050831     D W035A           s             35
003400101117      *
003500101117      *  Invio E-mail
003600101117     D Indirizzi       s            253
003700101117     D Oggetto         s             44
003800101117     D Messaggio       s           5000
003900101117      *
004000101117      *---------------------------------------------------------------------------
004100101117     d bart_it         C                   CONST('@Bartolini.it;')
004200101117     d CED_Bart        C                   CONST('CED@Bartolini.it;')
004300101117     d CED_Alert       C                   CONST('CedAlert@Bartolini.it;')
004400101117     d AB_mail         C                   CONST('Andrea.Bertocchi')
004500050831      *---------------------------------------------------------------------------
004600050831     C*? Seleziona il Flat File
004700050831     c                   if        sndrcv = 'S'
004800050901     c                   open      wfsnd00w
004900050831     c                   exsr      filesnd
005000050831     c                   else
005100050901     c                   open      wfrcv00w
005200050831     c                   exsr      filercv
005300050831     c                   endif
005400050831
005500050831     C                   SETON                                        LR
005600050831     C*------------------------------------------------------------
005700050831     c     filesnd       begsr
005800050831     C*------------------------------------------------------------
005900050831     C                   READ      wfsnd00w
006000050831     C                   If        not %EOF(wfsnd00w)
006100050901     c                   movel     flatrec       wdat
006200050831     c                   movel     flatrec       edtt00ds
006300050831     c                   exsr      repcli
006400050831     c                   endif
006500050901     c                   move      sumtpr        tiporcv
006600050831     c                   endsr
006700050831     C*
006800050831     C*------------------------------------------------------------
006900050831     c     filercv       begsr
007000050831     C*------------------------------------------------------------
007100050829     C                   READ      wfrcv00w
007200101117      *
007300050829     C                   If        not %EOF(wfrcv00w)
007400050829     c                   movel     flatrec       wdat
007500050829     c                   endif
007600101117      *
007700050901     c                   move      sumtpr        tiporcv
007800101117      *
007900101117      *  se il tipo non è previsto invia mail a CEDALERT di avviso sul WFRCV00W
008000101117      *  PER INTERVENIRE ed eliminare le righe deletandole fisicamente x poter
008100101117      *  far ricevere i messaggi EDI.
008200101117     c                   if        tiporcv <> 'TT00' and
008300101117     c                             tiporcv <> 'ST00'
008400101117     c                   exsr      Invio_Mail
008500101117     c                   end
008600101117      *
008700050831     c                   endsr
008800050831     C*------------------------------------------------------*
008900050831      * Cerca il codice del Cliente/Partner da Tab PT
009000050831     C*------------------------------------------------------*
009100050831     C     repcli        BEGSR
009200050831      *
009300050831      * Prova con il codice + qualificatore
009400050831     C                   MOVEL     TA0010        WCOD             35
009500050831     C                   MOVE      TAA007        WCOD
009600050831     C                   Z-ADD     1             XX                3 0
009700050831     C     WCOD          LOOKUP    CPT(XX)                                32
009800050831      *
009900050831      * se non l'ha trovato prova parzialmente senza il qualificatore
010000050831     C                   If        *IN32 = *off
010100050831     C                   MOVEL     ta0010        WCOD
010200050831     C                   eval      XX = 1
010300050831     C                   clear                   W035A
010400050831     C                   eval      W035A = %subst(WCod:1:31)
010500050831     C     W035A         lookup    CPTparz(XX)                            32
010600050831     C                   Endif
010700050831      *
010800050831      * Se ha trovato la tabella prende il codice del cliente e lo passa
010900050831      *  nel campo generico di INET
011000050831     C                   If        *IN32 = *on
011100050831     c                   movel     DPT(XX)       edidspt
011200050831     c                   movel     DPu(XX)       edidspu
011300050831     c                   move      *zeros        Clisnd
011400050831     c                   move      §PtKSC        Clisnd
011500050831     c                   move      §Pusig        sigsnd
011600050831     C                   Endif
011700050831      *
011800050831     C                   ENDSR
011900960530     C*
012000960530     C*----------------------------------------------------------------
012100960530     C*  *INZSR - OPERAZIONI INIZIALI
012200960530     C*----------------------------------------------------------------
012300960530     C     *INZSR        BEGSR
012400960530     C*
012500960530     C     *ENTRY        PLIST
012600050831     C                   PARM                    sndrcv
012700050831     C                   PARM                    tiporcv
012800050901     C                   PARM                    clisnd
012900050901     C                   PARM                    sigsnd
013000050901     c                   clear                   tiporcv
013100050901     c                   clear                   clisnd
013200050901     c                   clear                   sigsnd
013300050831      * ----------------
013400050831      * Caricamento Tabella Partner esteri
013500050831     C                   Z-ADD     0             X                 3 0
013600050831     C                   MOVEL     'PT'          TABCOD
013700050831     C     TABCOD        CHAIN     EDTAB01L                           30
013800050831     C     *IN30         DOWEQ     '0'
013900050831     C     TABFLG        ifeq      *BLANKS
014000050831     C                   ADD       1             X
014100050831     C                   MOVEL     TABKEY        CPT(X)
014200050831     C                   eval      CPTparz(X) = %subst(TABKEY:1:30)
014300050831     C                   MOVEL     TABUNI        DPT(X)
014400050831     C                   end
014500050831     C     TABCOD        READE     EDTAB01L                               30
014600050831     C                   END
014700121105      *
014800121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
014900121105     c                   exsr      ChecK_PT
015000121105      *
015100050901      * tab/PU
015200050901     C                   Z-ADD     0             X                 3 0
015300050901     C                   MOVEL     'PU'          TABCOD
015400050901     C     TABCOD        CHAIN     EDTAB01L                           30
015500050901     C     *IN30         DOWEQ     '0'
015600050901     C     TABFLG        ifeq      *BLANKS
015700050901     C                   ADD       1             X
015800050901     C                   MOVEL     TABUNI        DPu(X)
015900050901     C                   end
016000050901     C     TABCOD        READE     EDTAB01L                               30
016100050901     C                   END
016200050901      *
016300960530     C                   ENDSR
016400101117     c*==================================================================*
016500101117      * Manda un Msg x E-mail
016600101117     c*==================================================================*
016700101117     c     Invio_Mail    begsr
016800101117      *
016900101117     c                   eval      indirizzi = CED_Alert
017000101117     C                   eval      wrkEml = Indirizzi
017100101117     c                   eval      Oggetto ='Problemi File WFRCV00W ricez.EDI: -
017200101117     c                             su 1°Rcd'
017300101117     C                   eval      Messaggio = 'Nel File WFRCV00W ricezione EDI-
017400101117     c                              sono presenti records NON riconosciuti per -
017500101117     c                             la traduzione. Se non vengono eliminati, tut-
017600101117     c                             ti i msg EDI da portare su AS andranno in er-
017700101117     c                             rore. Eliminare le righe NON appartenenti a -
017800101117     c                             EDI.'
017900101117      *
018000101117     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
018100101117     C* Inizializzo variabili
018200101117     C                   movel     *blanks       wrkEml          253
018300101117     C                   movel     *blanks       wrkMsg         5000
018400101117     C                   movel     *blanks       wrkOgg           44
018500101117      *
018600101117     C* Valorizzo i campi della e-m@ail - indirizzo
018700101117     C                   eval      wrkEml = Indirizzi
018800101117     C                   eval      wrkOgg = Oggetto
018900101117     C                   eval      wrkMsg = Messaggio
019000110203      ********
019100110203     C********           call(e)   'TIS701C'
019200110203     C********           parm                    wrkEml
019300110203     C********           parm                    wrkOgg
019400110203     C********           parm                    wrkMsg
019500110203     C*
019600110203      ** *****
019700110203     C                   call(e)   'TRTCT00R2'
019800110203     C                   parm                    wrkEml
019900110203     C                   parm                    wrkOgg
020000110203     C                   parm                    wrkMsg
020100110203     C*
020200101117     C*
020300101117     C                   ENDSR
020400121105      *---------------------------------------------------------------*
020500121105      * CTRL caricamento schiere    PT                               -*
020600121105      *---------------------------------------------------------------*
020700121105     C     Check_PT      begsr
020800121105     C*
020900121105     c* Controllo riempimento schiera
021000121105     c                   clear                   trul0sds
021100121105     c                   eval      i0sski='CPT'
021200121105     c                   eval      i0sele=%elem(CPT)
021300121105     c                   eval      i0spie=x
021400121105     c                   eval      i0sfile='EDTAB00F'
021500121105     c                   eval      i0ssif=knsif
021600121105     c                   eval      i0spgm='TRTC100AR'
021700121112     c                   movel     kpjbu         kpjbusav
021800121112     c                   clear                   kpjbu
021900121105     c                   movel     trul0sds      kpjbu
022000121105     c                   call      'TRUL0SR'
022100121105     c                   parm                    kpjba
022200121112     c                   movel     kpjbusav      kpjbu
022300121105     C*
022400121105     C                   ENDSR
022500101117     ***********************************************************************
