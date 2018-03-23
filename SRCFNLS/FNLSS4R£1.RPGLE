000100051019     H DECEDIT('0,') DATEDIT(*DMY.)
000200051019     F* FNLSS4R *-----------------------------------------------------*
000300051019     F*       Stampa segnacolli export dpd in partenza
000400051019     FFNLSS4D   CF   E             WORKSTN
000500051019     FFNBLP01L  IF   E           K DISK
000600051019     FAzorg01L  IF   E           K DISK
000700051020     FTABEL00F  IF   E           K DISK
000800051019     d*
000900051019     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
001000051019     d comdrt          s                   Like(g02inv)
001100051020     D KCOD            S                   LIKE(TBLCOD) inz
001200051020     D KKEY            S                   LIKE(TBLKEY) inz
001300051019     d*
001400051021     D pog             S              3  0 DIM(30)
001500051019     d Azuteds       e ds                  Extname(Azute00f)
001600051019     d dDatiute      e ds
001700051019     d Tibs34ds      e ds                  Inz
001800051020     D* DS PER TRUL90R - RICHIESTA STAMPANTI
001900051020     D Trul90ds      e ds                  inz
002000051019      * ds per trul06r
002100051019     d trul06ds      e ds
002200051019     d  lin                    1     90  0 dim(30)
002300051019     d  dcli         E DS
002400051020     D DS3A          E DS
002500051019
002600051019     d  og143        E DS
002700051019     D* DS PER TIBS02R - GESTIONE TNTBE00F
002800051019     D tibs02ds      E DS
002900051019     d fnlss4ds      e ds
003000051019     D kpjba         E DS
003100051019     D***
003200051019     D WLBDAT          DS                  INZ
003300051019     D  G02DAT                 1      8  0
003400051019     D  G02INV                 9     16  0
003500051019     D  G02ERR                17     17
003600051019     D  G02TGI                18     22  0
003700051019
003800051019
003900051019     C     *entry        plist
004000051019     C                   parm                    kpjba
004100051019     c     *dtaara       define    §azute        azuteds
004200051019     c     *dtaara       define    §datiute      ddatiute
004300051019     c                   in(E)     *dtaara
004400051019     c                   If        %error  or RSUT = *blanks
004500051019     c                   CLEAR                   tibs34ds
004600051019     c                   CALL      'TIBS34R'
004700051019     c                   PARM                    tibs34ds
004800051019     c                   in        *dtaara
004900051019     c                   EndIf
005000051019     c*
005100051019     C* CARICO FILIALI GESTITE £1
005200051019     C*
005300051019     c                   clear                   trul06ds
005400051019     c                   if        dutlpo = '2'
005500051019     c                   movel     dutpou        d06key
005600051019     c                   eval      d06cod = '£6'
005700051019     c                   else
005800051019     c                   movel     simfel        d06key
005900051019     c                   eval      d06cod = '£1'
006000051019     c                   endif
006100051019     c                   movel     trul06ds      kpjbu
006200051019     c                   call      'TRUL06R'
006300051019     c                   parm                    kpjba
006400051019     c                   movel     kpjbu         trul06ds
006500051021     c                   movel     lin           pog
006600051019     c
006700051019     c     knsp          klist
006800051019     c                   kfld                    v1ssaa
006900051019     c                   kfld                    v1ssln
007000051019     c                   kfld                    v1sssr
007100051019     c                   kfld                    v1sssp
007200051020     C* Chiave di accesso per il file tabelle
007300051020     C     KTAB1         KLIST
007400051020     C                   KFLD                    CODUT             1 0
007500051020     C                   KFLD                    KCOD
007600051020     C                   KFLD                    KKEY
007700051021     c                   z-add     1             codut
007800051020     c*
007900051020     C                   movel     'S'           D90rse
008000051020     C                   call      'TRUL90R'
008100051020     C                   PARM                    KPJBA
008200051020     C                   parm                    Trul90ds
008300051020     C**
008400051020     C* F3 - FINE
008500051020     C                   IF        D90f3 = *on
008600051020     C                   GOTO      FINE
008700051020     C                   ENDIF
008800051019     c
008900051019     c* inizializzo videata da emettere
009000051019     c                   exsr      inzvid
009100051019     c     leggi         tag
009200051019     c                   exfmt     lss4d01
009300051019     c                   setoff                                       2890
009400051019     c                   if        *inkc = *on
009500051019     c                   goto      fine
009600051019     c                   endif
009700051019     c                   exsr      ctrd01
009800051019     c  nkf
009900051019     cor 90              goto      leggi
010000051019     c*
010100051019     c                   exsr      confe
010200051019     c
010300051019     c*
010400051019     c     fine          tag
010500051019     c                   seton                                        lr
010600051019     c**********************************************************************
010700051019     c* INZVID - inizializzazione campi videata
010800051019     c**********************************************************************
010900051019     C     inzvid        begsr
011000051019     c                   clear                   v1sdrt
011100051019     c                   clear                   v1sccm
011200051019     c                   z-add     999           v1sctr
011300051019     c                   clear                   v1slnp
011400051019     c                   clear                   v1slna
011500051021     c                   time                    wdate            14 0
011600051021     c                   move      wdate         v1ssaa
011700051019     C                   endsr
011800051019     c**********************************************************************
011900051019     c* CTRD01 - Controlli
012000051019     c**********************************************************************
012100051019     C     ctrd01        begsr
012200051019     c                   setoff                                       404142
012300051019     c                   setoff                                       434445
012400051019     c* verifico la selezione richiesta: o data ritiro o numero spedizione
012500051019     c                   if        v1sdrt = *zeros and v1ssln = *zeros
012600051019     c                   seton                                        289040
012700051019     c                   movel     msg(1)        v1cmsg
012800051019     c                   goto      endctr
012900051019     c                   endif
013000051019     c* troppe selezioni
013100051019     c                   if        v1sdrt > *zeros and v1ssln > *zeros
013200051019     c                   seton                                        289040
013300051019     c                   movel     msg(1)        v1cmsg
013400051019     c                   goto      endctr
013500051019     c                   endif
013600051019     c* SELEZIONE PER DATA RITIRO
013700051019     c                   if        v1sdrt > *zeros
013800051019     c                   clear                   comdrt
013900051019     c* controllo data ritiro
014000051019     C                   Z-ADD     V1sDRT        G02DAT
014100051019     C                   CLEAR                   G02ERR
014200051019     C                   CALL      'XSRDA8'
014300051019     C                   PARM                    WLBDAT
014400051019    1C     G02ERR        IFEQ      '1'
014500051019     C                   MOVEL     msg(2)        v1cmsg
014600051019     C                   SETON                                        412890
014700051019    1C                   ENDIF
014800051019     C  N90              MOVE      G02INV        COMDRT
014900051019     C  N90              MOVE      G02DAT        v1sdrt
015000051019     c* controllo cod.cliente mittente
015100051021     C                   CLEAR                   v1dccm
015200051021     C                   CLEAR                   dcli
015300051021     c***  '?'           scan      v1sccm                                 30
015400051021     c***                if        *in30 = *on
015500051021     c***                clear                   tibs02ds
015600051021     c***                Eval      T02Mod = 'R'
015700051021     c***                Eval      T02Sif = Knsif
015800051021     c***                Eval      T02Cod = 'CLI'
015900051021     c***                Call      'TIBS02R'
016000051021     c***                Parm                    Kpjba
016100051021     c***                Parm                    tibs02ds
016200051021     c***                Movel     T02Ke1        V1sccm
016300051021     c***                movel     t02uni        dcli
016400051021     c***                movel     §clirag       v1dccm
016500051021     c***                seton                                        90
016600051021     c***                goto      endctr
016700051021     c***                endif
016800051021     c                   if        v1sccm = *zeros
016900051019     c                   seton                                        902841
017000051019     c                   movel     msg(3)        v1cmsg
017100051019     c                   goto      endctr
017200051019     c                   endif
017300051019     c* se immesso deve essere presente in tabella cli e abilitato alla
017400051019     c* stampa segnacolli export dpd
017500051019     c                   if        v1sccm > *zeros
017600051019     c                   clear                   tibs02ds
017700051019     c                   clear                   dcli
017800051019     C                   MOVEL     'C'           T02MOD
017900051019     C                   MOVEL     KNSIF         T02SIF
018000051019     C                   MOVEL     'CLI'         T02COD
018100051019     c                   movel(P)  v1sccm        t02ke1
018200051019     C                   CALL      'TIBS02R'
018300051019     C                   PARM                    KPJBA
018400051019     C                   PARM                    tibs02ds
018500051019    2C                   IF        T02ERR = *BLANKS
018600051019     C                   MOVEL     T02uni        dcli
018700051021     c                   movel     §clirag       v1dccm
018800051019    2C                   ENDIF
018900051019     c                   if        t02err <> *blanks or §clidpd <> 'S'
019000051019     c                   seton                                        902841
019100051019     c                   movel     msg(4)        v1cmsg
019200051019     c                   goto      endctr
019300051019     c                   endif
019400051019     c                   endif
019500051019     c* Per il momento sul codice tariffa non faccio controlli
019600051019     c* controllo lnp se immessa
019700051019     c                   if        v1slnp > *zeros
019800051019     c     v1slnp        chain     azorg01l
019900051019     c                   if        not %found(azorg01l)
020000051019     c                   seton                                        432890
020100051019     c                   movel     msg(5)        v1cmsg
020200051019     c                   goto      endctr
020300051019     c                   endif
020400051021     c     v1slnp        lookup    pog                                    30
020500051021     c                   if        *in30 = *off
020600051021     c                   seton                                        289043
020700051021     c                   movel     msg(7)        v1cmsg
020800051021     c                   goto      endctr
020900051021     c                   endif
021000051019     c                   endif
021100051019     c* controllo lna se immessa
021200051019     c     v1slna        ifgt      *zeros
021300051019     c                   clear                   og143
021400051019     c     v1slna        chain     azorg01l
021500051019     c                   movel     orgde3        og143
021600051019     c                   if        not %found(azorg01l) or
021700051019     c                             §ogntw<>'DPD'
021800051019     c                   seton                                        442890
021900051019     c                   movel     msg(6)        v1cmsg
022000051019     c                   goto      endctr
022100051019     c                   endif
022200051019     c                   endif
022300051019     c
022400051019     c                   else
022500051019     c* SELEZIONE PER NUMERO SPEDIZIONE
022600051019     c* deve essere una spedizione esistente, export dpd e di competenz
022700051019     c* del p.o. che richiede la stampa
022800051021     c     v1ssln        lookup    pog                                    30
022900051019     c                   if        *in30 = *off
023000051019     c                   seton                                        289045
023100051019     c                   movel     msg(7)        v1cmsg
023200051019     c                   goto      endctr
023300051019     c                   endif
023400051021     c     knsp          chain     fnblp01l
023500051019     c                   if        %found(fnblp01l)
023600051020     c* errore se spedizione di recupero
023700051020     C                   MOVE      '3A'          KCOD
023800051020     C                   MOVE      *BLANKS       KKEY
023900051020     C                   MOVEL     BLPCBO        KKEY
024000051021     c                   clear                   ds3a
024100051021     C     KTAB1         CHAIN     TABEL00F
024200051021     C                   MOVEL     TBLUNI        DS3A
024300051020     c                   if        §3arbl = 'R'
024400051020     c                   seton                                        289045
024500051020     c                   movel     msg(9)        v1cmsg
024600051020     c                   goto      endctr
024700051020     c                   endif
024800051019     c                   endif
024900051021     c                   clear                   og143
025000051021     c     blplna        chain     azorg01l
025100051021     c                   movel     orgde3        og143
025200051019     c                   if        not %found(fnblp01l)
025300051019     c                             or §ogntw <> 'DPD'
025400051019     c                   seton                                        289045
025500051019     c                   movel     msg(8)        v1cmsg
025600051019     c                   goto      endctr
025700051019     c                   endif
025800051019     c                   endif
025900051019     C     endctr        endsr
026000051019     c**********************************************************************
026100051019     C* CONFE  - Richiamo pgm batch
026200051019     c**********************************************************************
026300051019     C     confe         begsr
026400051019     c                   clear                   fnlss4ds
026500051020     c                   movel     d90pss        i90pss
026600051020     c                   movel     d90mde        i90mde
026700051020     c                   movel     d90pre        i90pre
026800051019     c                   z-add     comdrt        is4drt
026900051019     c                   movel     v1sccm        is4ccm
027000051019     c                   z-add     v1sctr        is4ctr
027100051019     c                   z-add     v1slnp        is4lnp
027200051019     c                   z-add     v1slna        is4lna
027300051019     c                   z-add     v1ssln        is4sln
027400051019     c                   z-add     v1sssr        is4ssr
027500051019     c                   z-add     v1sssp        is4ssp
027600051019     c                   z-add     v1ssaa        is4saa
027700051019     c                   movel     fnlss4ds      kpjbu
027800051020     c                   movel     'LSS5'        kcoaz
027900051021     c                   call      'BCH10'
028000051021     c****               call      'FNLSS5C'
028100051019     c                   parm                    kpjba
028200051019     C                   endsr
028300051019** MSG
028400051019Immettere una selezione: o per data ritiro o per numero spedizione            01
028500051019Data ritiro errata                                                            02
028600051019Codice cliente mittente obbligatorio                                           3
028700051019Cod.Mittente non abilitato alla stampa in part.dei segnacolli export DPD       4
028800051019P.O. di partenza inesistente                                                   5
028900051019P.O. di arrivo immesso non è un P.O. DPD                                       6
029000051019P.O. di partenza non gestibile dall'utente                                     7
029100051019Spedizione inesistente o non export DPD                                        8
029200051020Spedizione di recupero
