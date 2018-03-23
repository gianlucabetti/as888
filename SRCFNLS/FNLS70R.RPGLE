000100050805     h decedit('0,') datedit(*dmy/)
000200050805
000300110111      * FNLS70R *-----------------------------------------------------*
000400110111      * CANCELLAZIONE DI EDIVAD/VAT/VAX SENZA IL RELATIVO EDIVAB      *
000500050805      *---------------------------------------------------------------*
000600050805
000700110111     fedivab4L  if   e           k disk
000800120510     F*---------
000900120510     FTNTBE01L  IF   E           K DISK
001000050805      *
001100110111     fedivad1L  Uf   e           k disk
001200110111     fedivat1L  Uf   e           k disk
001300110111     fedivax1L  Uf   e           k disk
001400050805
001500050808      *
001600050805      * STRUTTURE DATI -----------------------------------------------*
001700050808      *
001800050805     d KPJBA         e ds
001900120424      *
002000120424      * Tabella 3BC = Bypass ctrl x cliente
002100120424     d d3BC          e ds                  inz
002200120510     D CCM             S              7  0 DIM(100)
002300120424      * DS PER TIBS02R - GESTIONE TNTBE00F
002400120424     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002500050805      *
002600050805      * VARIABILI ----------------------------------------------------*
002700050808      *
002800050805     d Kfgs            s                   like(VABfgs) inz
002900120510     D wY              s              3s 0
003000050808      * - Flags booleani
003100050805     d $Found          s              1    inz(*off)
003200050805      *
003300050805      * KEY - LIST ---------------------------------------------------*
003400050808      *
003500110111     c     K04VAB04      klist
003600110111     c                   kfld                    VABcmr
003700110111     c                   kfld                    VABccm
003800110111     c                   kfld                    VABcnt
003900110111     c                   kfld                    VABaas
004000110111     c                   kfld                    VABlnp
004100110111     c                   kfld                    VABnrs
004200110111     c                   kfld                    VABnsp
004300050805
004400050805      *---------------------------------------------------------------*
004500050805      *
004600050808      * - Elaborazione FIVAD01L
004700110111     c                   exsr      ElabVAD
004800050808      * - Elaborazione FIVAT01L
004900110111     c                   exsr      ElabVAT
005000050808      * - Elaborazione FIVAX01L
005100110111     c                   exsr      ElabVAX
005200050805      *
005300050808      * Fine
005400050805     c                   eval      *inLR  = *on
005500050805      *
005600050805      *---------------------------------------------------------------*
005700110111      * Elaborazione file EDIVAD1L                                    *
005800050805      *---------------------------------------------------------------*
005900110111     c     ElabVAD       BEGSR
006000050808      *
006100110111     c     *loval        setll     edivad00
006200110111     c                   read      edivad00
006300050805      *
006400050805      * Elaborazione
006500110111do  1c                   DOW       NOT %eof(edivad1L)
006600050905      *
006700050905     c                   eval      VABaas   = VADaas
006800050905     c                   eval      VABlnp   = VADlnp
006900050905     c                   eval      VABnrs   = VADnrs
007000050905     c                   eval      VABnsp   = VADnsp
007100050905     c                   eval      Kfgs     = VADfgs
007200110111     c                   eval      VABccm   = VADccm
007300110111     c                   eval      VABcmr   = VADcmr
007400110111     c                   eval      VABcnt   = VADcnt
007500120510     C*
007600120510     C* controllo se esiste un bypass su questo cliente (solo NON se c'è cerco i
007700120510     C*  dati di testata, altrimenti non serve perché comunque NON cancellerei)
007800120510     C                   IF        %lookup(VABCCM : CCM) = 0
007900110111     c                   exsr      CercaVAB
008000050905if  3c                   if        $Found   = *off
008100110111     c                   DELETE    EDIVAD00
008200050905e   3c                   endif
008300120510     C                   ENDIF
008400050905      *
008500110113     c                   read      EDIVAD00
008600050805      *
008700050805e   1c                   ENDDO
008800050805      *
008900050805     c                   ENDSR
009000050805      *
009100050805      *---------------------------------------------------------------*
009200110111      * Elaborazione file EDIVAT1L                                    *
009300050805      *---------------------------------------------------------------*
009400110111     c     ElabVAT       BEGSR
009500050805      *
009600110111     c     *loval        setll     edivat00
009700110111     c                   read      edivat00
009800050805      *
009900050805      * Elaborazione
010000110111do  1c                   DOW       NOT %eof(EDIVAT1L)
010100050805      *
010200050905     c                   eval      VABaas   = VATaas
010300050905     c                   eval      VABlnp   = VATlnp
010400050905     c                   eval      VABnrs   = VATnrs
010500050905     c                   eval      VABnsp   = VATnsp
010600050905     c                   eval      Kfgs     = VATfgs
010700110111     c                   eval      VABccm   = VATccm
010800110111     c                   eval      VABcmr   = VATcmr
010900110111     c                   eval      VABcnt   = VATcnt
011000120510     C*
011100120510     C* controllo se esiste un bypass su questo cliente (solo NON se c'è cerco i
011200120510     C*  dati di testata, altrimenti non serve perché comunque NON cancellerei)
011300120510     C                   IF        %lookup(VABCCM : CCM) = 0
011400110111     c                   exsr      CercaVAB
011500050905if  3c                   if        $Found   = *off
011600110111     c                   DELETE    edivat00
011700050905e   3c                   endif
011800120510     C                   ENDIF
011900050905      *
012000110111     c                   read      EDIVAT00
012100050805      *
012200050805e   1c                   ENDDO
012300050805      *
012400050805     c                   ENDSR
012500050805      *
012600050805      *---------------------------------------------------------------*
012700110111      * Elaborazione file EDIVAX1L                                    *
012800050805      *---------------------------------------------------------------*
012900110111     c     ElabVAX       BEGSR
013000050805      *
013100110111     c     *loval        setll     EDIVAX00
013200110111     c                   read      EDIVAX00
013300050805      *
013400050805      * Elaborazione
013500110111do  1c                   DOW       NOT %eof(EDIVAX1L)
013600050805      *
013700050905     c                   eval      VABaas   = VAXaas
013800050905     c                   eval      VABlnp   = VAXlnp
013900050905     c                   eval      VABnrs   = VAXnrs
014000050905     c                   eval      VABnsp   = VAXnsp
014100050905     c                   eval      Kfgs     = VAXfgs
014200110111     c                   eval      VABccm   = VAXccm
014300110111     c                   eval      VABcmr   = VAXcmr
014400110111     c                   eval      VABcnt   = VAXcnt
014500120510     C*
014600120510     C* controllo se esiste un bypass su questo cliente (solo NON se c'è cerco i
014700120510     C*  dati di testata, altrimenti non serve perché comunque NON cancellerei)
014800120510     C                   IF        %lookup(VABCCM : CCM) = 0
014900110111     c                   exsr      CercaVAB
015000050905if  3c                   if        $Found   = *off
015100110111     c                   DELETE    EDIVAX00
015200050905e   3c                   endif
015300120510     C                   ENDIF
015400050905      *
015500110111     c                   read      EDIVAX00
015600050805      *
015700050805e   1c                   ENDDO
015800050805      *
015900050805     c                   ENDSR
016000050808      *
016100050808      *---------------------------------------------------------------*
016200110111      * Ricerca record corrispondenti in EDIVAB4L                     *
016300050808      *---------------------------------------------------------------*
016400110111     c     CercaVAB      BEGSR
016500050808      *
016600050808     c                   eval      $Found = *off
016700050808      *
016800110111     c     K04VAB04      setll     EDIVAB00
016900110111     c     K04VAB04      reade     EDIVAB00
017000050808      *
017100110111do  1c                   DOW       NOT %eof(EDIVAB4L)
017200050808      *
017300110111     c                   if        VABfgs = Kfgs
017400050808     c                   eval      $Found = *on
017500050808     c                   leave
017600050808e   2c                   endif
017700050808      *
017800110111     c     K04VAB04      reade     edivab00
017900050808      *
018000050808e   1c                   ENDDO
018100050808      *
018200050808     c                   ENDSR
018300120510     C*----------------------------------------------------*
018400120510     C*   *INZSR: Operazioni iniziali                      *
018500120510     C*----------------------------------------------------*
018600120510     C     *INZSR        BEGSR
018700120510     C*
018800120510     C     *ENTRY        PLIST
018900120510     C                   PARM                    KPJBA
019000120510      *
019100120510      * Caricamento Tabella 3BC bypass ctrl
019200120510     C                   Z-ADD     0             wY
019300120510     C     '3BC'         CHAIN     TNTBE01L                           31
019400120510      *
019500120510     C     *IN31         DOWEQ     '0'
019600120510      * Se il S.I. è indicato deve essere uguale al mio
019700120510     C     TBEATB        IFEQ      ' '
019800120510     C     TBEKE2        ANDEQ     *blank
019900120510     C     TBESIF        IFEQ      KNSIF
020000120510     C     TBESIF        OREQ      *BLANK
020100120510     C                   MOVEL     TBEUNI        D3BC
020200120510      * salvo il  cliente solo se il bypass relativo alla NON cancellazione
020300120510      * dati in mancanza del VAB è attivo
020400120510     C                   IF        §3BCDNT = 'S'
020500120510     c                   if        %lookup(%dec(%trim(%subst(TBEKe1 : 1 : 7))
020600120510     C                              : 7 : 0) : CCM) = 0
020700120510     C                   ADD       1             wY
020800120510     C                   EVAL      CCM(wY) =
020900120510     C                             %dec(%trim(%subst(TBEKe1 : 1 : 7)) : 7 : 0)
021000120510     c                   end
021100120510     C                   ENDIF
021200120510     C                   END
021300120510     C                   END
021400120510     C     '3BC'         READE     TNTBE01L                               31
021500120510     C                   ENDDO
021600120510     C*
021700120510     C                   ENDSR
