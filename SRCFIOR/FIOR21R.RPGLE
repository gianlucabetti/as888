000100010418     H DECEDIT('0,') DATEDIT(*DMY.)
000200010418
000300010418      *****************************************************************
000400010418      *                                                               *
000500010418      *       STAMPA ANAGRAFICA RITIRI FISSI                          *
000600010418      *                                                               *
000700010418      *****************************************************************
000800010418
000900010418     FAZORG01L  IF   E           K DISK
001000010418     FFIOR21D   CF   E             WORKSTN
001100130620
001200130620     d wabi            s                   like(UTEaut)
001300130620     d wEsci           s               n   inz(*off)
001400130620     d wErrGrave       s               n   inz(*off)
001500130620     d w003a           s              3
001600010418
001700020809     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             Msg video
001800130620     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
001900010418
002000010418     D KPJBA         E DS
002100010418     D FIOR21DS      E DS
002200010418
002300130620     d azuteds       e ds                  extname(azute00f)
002400130620     d ddatiute      e ds
002500130620     d TIBS02DS      e ds
002600130620     d tibs34ds      e ds
002700130620     d TRUL31DS      e ds
002800130620     d dLAT          e ds
002900130620     d dUTE01        e ds
003000010418
003100010418     D                SDS
003200010418     D  V1CPGM                 1     10
003300010418      *--------------------------------------------------------------**
003400010418      * INDICATORI                                                   **
003500010418      *--------------------------------------------------------------**
003600010418      *  01  - RICHIESTA RICERCA ALFABETICA
003700010418      *  02  - VARIAZIONE
003800010418      *  28  - Visualizzo messaggio Errore
003900010418      *  30  - Comodo
004000010418      *--------------------------------------------------------------**
004100010418
004200010418     C                   CLEAR                   V1cMSG
004300010418     C                   MOVEL     '1'           V1oRD
004400010418     C                   clear                   V1mp
004500010418     C                   CLEAR                   V1poe
004600010418     C                   CLEAR                   V1dpoe
004700010418
004800010418     C                   do        *hival
004900130620
005000130620      /free
005100130620       //?Se utente non autorizzato alla funzione esco
005200130620         IF  wErrGrave;
005300130620           *in28 = *on;
005400130620           wEsci = *on;
005500130620           V1Cmsg = 'Utente non autorizzato alla funzione';
005600130620         ENDIF;
005700130620      /end-free
005800010418     C                   EXFMT     FI21D01
005900010418     C                   SETOFF                                         28
006000130620
006100130620      /free
006200130620         IF  wEsci;
006300130620           leave;
006400130620         ENDIF;
006500130620      /end-free
006600010418
006700010418      * F3=FINE
006800010418     C                   if        *inkc = *on
006900010418     C                   leave
007000010418     C                   endif
007100010418      * Controllo
007200010418     C                   EXSR      CTRD02
007300000309     C  NKF
007400010418     COR 28              iter
007500010418
007600010418      * F6 = CONFERMA
007700010418     C                   CLEAR                   fior21ds
007800010418     C                   MOVEL     V1ord         d21ORD
007900010418     C                   Z-ADD     V1por         d21POR
008000010418     C                   MOVEL     V1mp          d21mp
008100010418     C                   Z-ADD     V1poe         d21poe
008200130620     c                   eval      d21abi = wabi
008300010418     C                   MOVEL     'OR22'        KCOAZ
008400010418     C                   MOVEL(P)  fior21ds      KPJBU
008500010418     C                   CALL      'BCH10'
008600010418     C**!!!              CALL      'FIOR22R'
008700010418     C                   PARM                    KPJBA
008800010502     C                   leave
008900010418
009000010418     C                   enddo
009100010418
009200010418     C                   eval      *inlr = *on
009300010418
009400010418      *---------------------------------------------------------------*
009500010418      *    Controlli videata                                          *
009600010418      *---------------------------------------------------------------*
009700961003     C     CTRD02        BEGSR
009800010418
009900000309     C                   SETOFF                                       0102
010000010418     C                   CLEAR                   V1dpor
010100010418     C                   CLEAR                   V1dpoe
010200010418
010300010418      * P.O. RITIRO: SE IMMESSO
010400010418     C                   if        v1por <> 0
010500010418     C     v1por         chain     azorg01l
010600010418     C                   if        %found(azorg01l)
010700010418     C                   movel(p)  orgdes        v1dpor
010800130620     c                   move      V1por         w003a
010900130620     c     w003a         lookup    SKpog                                  30
011000010418     C                   if        *in30 = *off
011100010418     C                   seton                                        01  28
011200010418     C                   movel     msg(2)        v1cmsg
011300010418     C                   goto      endctrd02
011400010418     C                   endif
011500010418     C                   else
011600010418     C                   seton                                        01  28
011700010418     C                   movel     msg(2)        v1cmsg
011800010418     C                   goto      endctrd02
011900010418     C                   endif
012000010418     C                   endif
012100010418
012200010418      * PO CLIENTE SE IMMESSO
012300010418     C                   if        v1poe <> *zeros
012400010418     C     v1poe         chain     azorg01l
012500010418     C                   if        %found(azorg01l)
012600010418     C                   movel(p)  orgdes        v1dpoe
012700010418     C                   else
012800010418     C                   seton                                        02  28
012900010418     C                   movel     msg(1)        v1cmsg
013000010418     C                   goto      endctrd02
013100010418     C                   endif
013200010418     C                   endif
013300010418
013400010418     C     endctrd02     ENDSR
013500010418      *****************************************************************
013600010418      * ROUTINE INIZIALE
013700010418      *****************************************************************
013800961003     C     *INZSR        BEGSR
013900010418
014000961003     C     *ENTRY        PLIST
014100961003     C                   PARM                    KPJBA
014200130620
014300130620     c     *dtaara       define    §azute        azuteds
014400130620     c     *dtaara       define    §datiute      ddatiute
014500130620     c                   in(e)     *dtaara
014600130620     c                   if        %error or rsut = *blanks
014700130620     c                   clear                   tibs34ds
014800130620     c                   call      'TIBS34R'
014900130620     c                   parm                    tibs34ds
015000130620     c                   in        *dtaara
015100130620     c                   endif
015200130620
015300130620      * controllo abilitazione utente
015400130620     c                   clear                   wabi
015500130620     c                   clear                   dLAT
015600130620     c                   eval      dUTE01 = UTEfaf
015700130620
015800130620      * Verifica errori e autorità profilo
015900130620     c                   SELECT
016000130620      * se ho errori nei dati utente esco dal pgm
016100130620     c                   WHEN      DUTerr = 'E'
016200130620     c                   eval      wErrGrave = *on
016300130620     c                   leavesr
016400130620      * carica abilitazioni del profilo
016500130620     c                   WHEN      §UTEorm <> *blanks
016600130620     c                   eval      wabi = §UTEorm
016700130620     c                   WHEN      UTEaut <> *blanks
016800130620     c                   eval      wabi = UTEaut
016900130620      * se non c'è l'abilitazione
017000130620      * --> se 1° livello, abilitazioni al terminal
017100130620      *     se 2° livello, abilitazioni al punto operativo
017200130620      *     se sede no abilitazioni
017300130620     c                   OTHER
017400130620     c                   IF        DUTlpo = '1'
017500130620     c                   eval      wabi   = 'TP'
017600130620     c                   ENDIF
017700130620     c                   IF        DUTlpo = '2'
017800130620     c                   eval      wabi   = 'PO'
017900130620     c                   ENDIF
018000130620     c                   ENDSL
018100130620
018200130620      * controllo se ok l'abilitazione dell'utente
018300130620     c                   clear                   TIBS02DS
018400130620     c                   eval      T02mod = 'C'
018500130620     c                   eval      T02sif = knsif
018600130620     c                   eval      T02cod = 'LAT'
018700130620     c                   eval      T02ke1 = wabi
018800130620     c                   Call      'TIBS02R'
018900130620     c                   Parm                    KPJBA
019000130620     c                   Parm                    TIBS02DS
019100130620     c                   IF        T02err <> *blanks or §LATabi = 'S'
019200130620     c                   eval      wErrGrave = *on
019300130620     c                   leavesr
019400130620     c                   ENDIF
019500130620     c                   eval      dLAT = T02uni
019600130620
019700130620      * Reperimento delle filiali gestibili dall'utente
019800130620     c                   clear                   TRUL31DS
019900130620     c                   eval      I31abi = wabi
020000130620     c                   eval      I31cdi = DUTdis
020100130620     c                   eval      I31car = DUTare
020200130620     c                   eval      I31cpo = DUTpou
020300130620     c                   call      'TRUL31R'
020400130620     c                   parm                    KPJBA
020500130620     c                   parm                    TRUL31DS
020600130620     c                   IF        O31pog > *zeros
020700130620     c                   movea     O31pog        SKpog
020800130620     c                   ELSE
020900130620     c                   eval      wErrGrave = *on
021000130620     c                   leavesr
021100130620     c                   ENDIF
021200020808
021300130702      * Imposto la filiale ritiro
021400130702     c                   Movel     DUTpou        V1por
021500020808      * Decodifico P.o. di gestione
021600130702     c     V1por         Chain     Azorg01L
021700020808     c                   If        Not%Found(Azorg01l)
021800130702     c                   Clear                   V1dpor
021900020808     c                   Else
022000130702     c                   Movel     OrgDES        V1dpor
022100020808     c                   EndIf
022200010418
022300961003     C                   ENDSR
022400000317**
022500070418Immettere una filiale emissione valida                                        1
022600070418Filiale ritiro non gestibile                                                  2
