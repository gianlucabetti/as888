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
002100990907     d*   DS contenente i dati dI INPUT     .
002200990907     d TIS172DSI     e DS
002300990907     D RIF             s                   like(rif01O72) based(Rifp)
002400990907     D                                     dim(RighePagina)
002500990907     D DAT             s                   like(dat01O72) based(datp)
002600990907     D                                     dim(RighePagina)
002700990907     D DES             s                   like(des01O72) based(desp)
002800990907     D                                     dim(RighePagina)
002900990907     D DST             s                   like(dst01O72) based(dstp)
003000990907     D                                     dim(RighePagina)
003100990907     D STA             s                   like(sta01O72) based(stap)
003200990907     D                                     dim(RighePagina)
003300990910     D SPE             s                   like(SPE01O72) based(spep)
003400990910     D                                     dim(RighePagina)
003500010125     D MIT             s                   like(Mit01O72) based(Mitp)
003600010125     D                                     dim(RighePagina)
003700990903     c
003800990903     c*
003900990903     c     *entry        plist
004000990907     c                   parm                    TIS172DSI
004100990903     c                   parm                    IdRichiesta
004200990903     c                   parm                    Operazione
004300990907     c                   parm                    Esito
004400990903     c                   parm                    PAGdati
004500990903     c*
004600990903     c*
004700990903     c*
004800990903     c* Operazione
004900990903     c* '1' = Inizio nuova richiesta
005000990903     c* '2' = dati trovati, richiesta successiva
005100990903     c* '9' = Non trovati dati
005200990903     c*
005300990903     c                   clear                   i
005400990907     c                   eval      Esito = '0'
005500990907
005600990907     c                   dow       Esito = '0' and i <  RighePagina
005700990907     C                             AND operazione <> '9'
005800991217     c                   call      'TIS773R'
005900990907     c                   parm                    TIS172DSI
006000990903     c                   parm                    Operazione
006100990907     c                   parm      Righepagina   RigheIn
006200990907     c                   parm                    TIS173DSO
006300990907     c                   parm                    RigheOut
006400990907     c                   parm                    Esito
006500990903
006600990907     c                   if        Esito = '0' and RigheOut > 0
006700990907     c* carica campi dalla TIS173DSO negli elementi (i) della TIS172DSO
006800990903     c*
006900990907     c                   do        Righeout      y
007000990907     c                   eval      i = i + 1
007100990907     c     y             occur     TIS173DSO
007200990907     c                   eval      RIF(i) = rifO73
007300990907     c                   eval      Dat(i) = datO73
007400990907     c                   eval      Des(i) = desO73
007500990907     c                   eval      Dst(i) = dstO73
007600990907     c                   eval      Sta(i) = StaO73
007700990910     c                   eval      Spe(i) = SpeO73
007800010125     c                   eval      Mit(i) = MitO73
007900990907     c                   enddo
008000990907     c                   endif
008100990907     c*
008200990907     c                   enddo
008300990907     c                   if        i > 0
008400990907     c                   eval      operazione = '2'
008500990907     c                   endif
008600990903     c*
008700990903     c* imposta numero elementi caricati = i
008800990907     C                   EVAL      RIGO72 = I
008900990903     c* imposta altri campi non schiera di DSPAG
009000990903     c*
009100990907     c                   eval      pagdati = tis172dSo
009200990907     C                   RETURN
009300990907     c*-----------------------------------------------------
009400990907     c*
009500990907     c     *inzsr        begsr
009600990907     c                   eval      Rifp = %ADDR(Rif01O72)
009700990907     c                   eval      Datp = %ADDR(Dat01O72)
009800990907     c                   eval      Desp = %ADDR(Des01O72)
009900990907     c                   eval      Dstp = %ADDR(Dst01O72)
010000990907     c                   eval      Stap = %ADDR(Sta01O72)
010100990910     c                   eval      Spep = %ADDR(Spe01O72)
010200010125     c                   eval      Mitp = %ADDR(Mit01O72)
010300990907     c                   endsr
