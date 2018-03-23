000100990907      /TITLE Compone pagina di TIVPG00F
000200990903     Hdatedit(*DMY)
000300990907     D Esito           s              1
000400990903     D PgmPagina       s             10
000500990907     D RighePAgina     C                   Const(20)
000600990903     D IdRichiesta     s              9  0
000700990903     D Operazione      s              1
000800990907     D Pagdati         s           9999
000900990907     D I               s              5i 0
001000990907     D Y               s              5i 0
001100990907     D RigheOUT        s              5i 0
001200990907     D RigheIn         s              5i 0
001300990903     d*
001400990903     d*   DS ritornata dal programma di accesso ai dati
001500990903     D*      contiene una riga di dati
001600990907     d TIS173DSO     e DS                  OCCURS(RIGHEPAGINA)
001700990903     c*
001800990903     d*   DS contenente i dati della pagina .
001900990903     D*      E' la DS attesa dal server di strategi
002000990907     d TIS172DSO     e DS
002100040408     D TIS776DSI     E DS                                                       Ricerca con chi sono
002200040408     D                                     INZ
002300990907     d*   DS contenente i dati dI INPUT     .
002400990907     d TIS172DSI     e DS
002500990907     D RIF             s                   like(rif01O72) based(Rifp)
002600990907     D                                     dim(RighePagina)
002700990907     D DAT             s                   like(dat01O72) based(datp)
002800990907     D                                     dim(RighePagina)
002900990907     D DES             s                   like(des01O72) based(desp)
003000990907     D                                     dim(RighePagina)
003100990907     D DST             s                   like(dst01O72) based(dstp)
003200990907     D                                     dim(RighePagina)
003300990907     D STA             s                   like(sta01O72) based(stap)
003400990907     D                                     dim(RighePagina)
003500990910     D SPE             s                   like(SPE01O72) based(spep)
003600990910     D                                     dim(RighePagina)
003700010125     D MIT             s                   like(Mit01O72) based(Mitp)
003800010125     D                                     dim(RighePagina)
003900990903     c
004000990903     c*
004100990903     c     *entry        plist
004200990907     c                   parm                    TIS172DSI
004300990903     c                   parm                    IdRichiesta
004400990903     c                   parm                    Operazione
004500990907     c                   parm                    Esito
004600990903     c                   parm                    PAGdati
004700990903     c*
004800990903     c*
004900990903     c*
005000990903     c* Operazione
005100990903     c* '1' = Inizio nuova richiesta
005200990903     c* '2' = dati trovati, richiesta successiva
005300990903     c* '9' = Non trovati dati
005400990903     c*
005500990903     c                   clear                   i
005600990907     c                   eval      Esito = '0'
005700990907
005800990907     c                   dow       Esito = '0' and i <  RighePagina
005900990907     C                             AND operazione <> '9'
006000040408
006100040408     C                   SELECT
006200040408     ** Ricerca con "chi sono".
006300040408     C                   WHEN      ChiSonoI72 <> *BLANK
006400040408     C
006500040408     C                   IF        KSCI72 = *BLANK
006600040408     C                   EVAL      AnonimoI76 = *ON                             Utente anonimo
006700040408     C                   EVAL      ParzialI76 = *OFF                            No ricerca parziale
006800040408     C                   ELSE
006900040408     C                   EVAL      AnonimoI76 = *OFF                            Utente iscritto
007000040408     C                   EVAL      ParzialI76 = *ON                             Sì ricerca parziale
007100040408     C                   ENDIF
007200040408     C
007300040408     C                   EVAL      KSCI76     = KSCI72
007400040408     C                   EVAL      ChiSonoI76 = ChiSonoI72
007500040408     C                   EVAL      DInI76     = DInI72
007600040408     C                   EVAL      DFiI76     = DFiI72
007700040408     C                   EVAL      RqsCIdI76  = RqsCIdI72
007800040408     C
007900040408     c                   CALL      'TIS776R'
008000040408     C                   PARM                    TIS776DSI
008100040408     C                   PARM                    Operazione
008200040408     C                   PARM      RighePagina   RigheIn
008300040408     C                   PARM                    TIS173DSO
008400040408     C                   PARM      *ZERO         RigheOut
008500040408     C                   PARM      *BLANK        Esito
008600040408     ** Ricerca con altri parametri.
008700040408     C                   OTHER
008800991217     c                   call      'TIS773R'
008900990907     c                   parm                    TIS172DSI
009000990903     c                   parm                    Operazione
009100990907     c                   parm      Righepagina   RigheIn
009200990907     c                   parm                    TIS173DSO
009300990907     c                   parm                    RigheOut
009400990907     c                   parm                    Esito
009500040408     C
009600040408     C                   ENDSL
009700990903
009800990907     c                   if        Esito = '0' and RigheOut > 0
009900990907     c* carica campi dalla TIS173DSO negli elementi (i) della TIS172DSO
010000990903     c*
010100990907     c                   do        Righeout      y
010200990907     c                   eval      i = i + 1
010300990907     c     y             occur     TIS173DSO
010400990907     c                   eval      RIF(i) = rifO73
010500990907     c                   eval      Dat(i) = datO73
010600990907     c                   eval      Des(i) = desO73
010700990907     c                   eval      Dst(i) = dstO73
010800990907     c                   eval      Sta(i) = StaO73
010900990910     c                   eval      Spe(i) = SpeO73
011000010125     c                   eval      Mit(i) = MitO73
011100990907     c                   enddo
011200990907     c                   endif
011300990907     c*
011400990907     c                   enddo
011500990907     c                   if        i > 0
011600990907     c                   eval      operazione = '2'
011700990907     c                   endif
011800990903     c*
011900990903     c* imposta numero elementi caricati = i
012000990907     C                   EVAL      RIGO72 = I
012100990903     c* imposta altri campi non schiera di DSPAG
012200990903     c*
012300990907     c                   eval      pagdati = tis172dSo
012400990907     C                   RETURN
012500990907     c*-----------------------------------------------------
012600990907     c*
012700990907     c     *inzsr        begsr
012800990907     c                   eval      Rifp = %ADDR(Rif01O72)
012900990907     c                   eval      Datp = %ADDR(Dat01O72)
013000990907     c                   eval      Desp = %ADDR(Des01O72)
013100990907     c                   eval      Dstp = %ADDR(Dst01O72)
013200990907     c                   eval      Stap = %ADDR(Sta01O72)
013300990910     c                   eval      Spep = %ADDR(Spe01O72)
013400010125     c                   eval      Mitp = %ADDR(Mit01O72)
013500990907     c                   endsr
