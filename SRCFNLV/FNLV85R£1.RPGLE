000100940222     H DECEDIT('0,') DATEDIT(*DMY.)
000200060207     H* FNLV85R  *----------------------------------------------------*
000300930406     H*------------------*                                            *
000400060207     H*        GESTIONE MOTIVO VARIAZIONE BOLLA                      **
000500921214     H*---------------------------------------------------------------*
000600060207     Ftabel00f  iF   E           K DISK
000700060209     Ffiar500f  O  A E             DISK
000800060207     FFNLV85D   CF   E             WORKSTN
000900921214     D*
001000060207     D msg             S             50    DIM(2) CTDATA PERRCD(1)
001100060207
001200060207     D fnlv85ds      e DS
001300940218     D*
001400060207     d DS7L          E DS
001500060207     D Dmtv          E DS
001600921214     D KPJBA         E DS
001700060207     D tibs02ds      E DS
001800060209     D dar5mtv       E DS
001900060209     D***
002000060209     D WLBDAT          DS                  INZ
002100060209     D  G02DAT                 1      8  0
002200060209     D  G02INV                 9     16  0
002300060209     D  G02ERR                17     17
002400060209     D  G02TGI                18     22  0
002500060207     d*
002600060207     D codut           S                   LIKE(TBLkut) inz(1)
002700060207     D kcod            S                   LIKE(TBLcod) inz('7L')
002800060207     D kkey            S                   LIKE(TBLkey)
002900921214     I*
003000940209     C*****************************************************************
003100940209     C* RIEPILOGO INDICATORI
003200940209     C*****************************************************************
003300940209     C*****************************************************************
003400940209     C     *ENTRY        PLIST
003500060207     C                   PARM                    kpjba
003600060208     C                   PARM                    ds7l
003700060207     c*
003800060207     c                   movel     kpjbu         fnlv85ds
003900060209     c*
004000060209     C                   TIME                    W0140            14 0
004100060209     C* UDATE IN GGMMAAAA
004200060209     C                   MOVE      W0140         WDTGIO            8 0
004300060209     C*
004400060209     C* UDATE IN AAAAMMGG
004500060209     C                   Z-ADD     WDTGIO        G02DAT
004600060209     C                   MOVEL     *BLANK        G02ERR
004700060209     C                   CALL      'XSRDA8'
004800060209     C                   PARM                    WLBDAT
004900060209     C                   MOVEL     G02INV        DATEU             8 0
005000060207     c
005100940210     C*
005200940209     C*---------------------------------------------------------------*
005300940209     C*
005400060207     C* ACCESSO Tabel00F
005500060207     C     KTab          KLIST
005600060207     C                   KFLD                    CODUT
005700060207     C                   KFLD                    kcod
005800060207     C                   KFLD                    Kkey
005900940210     C*
006000921214     C*---------------------------------------------------------------*
006100060207      *    tla = ' '  Elaboro e chiudo con RETURN
006200060207      *    tla = 'L'  Elaboro e chiudo con LR
006300060207      *    tla = 'C'            chiudo con LR  solo per chiusura file
006400021022      *
006500060207    1c                   if        Ilv85tla = 'C'
006600060207     c                   eval      *inlr = *on
006700060207
006800060207   X1c                   else
006900060207     c*
007000060207     c* pulizia campi di output
007100060207     c                   clear                   olv85mtv
007200060207     c                   clear                   olv85nt1
007300060207     c                   clear                   olv85nt2
007400060207     c                   clear                   olv85f12
007500060209     c*
007600060209     c                   select
007700060209     c                   when      ilv85tel = 'V'
007800060209     c                   exsr      gesvid
007900060209     c                   when      ilv85tel = 'R'
008000060209     c                   exsr      geswrt
008100060209     c                   endsl
008200060207     c*
008300921214     C*
008400060207     c                   movel     fnlv85ds      kpjbu
008500060207    2c                   if        Ilv85tla = 'L'
008600060207     c                   eval      *inlr = *on
008700060207   X2c                   else
008800060207     c                   return
008900060207    2c                   endif
009000940222     C*
009100060207    1c                   endif
009200060207     C*****************************************************************
009300060207     c     *inzsr        begsr
009400060207     C*****************************************************************
009500060207     c*
009600060207     c* Pulizia campi del video (solo la prima volta per mantenere i dati
009700060207     c* immessi fino a nuova bolla). Quindi a fine bolla chiudere sempre
009800060207     c* questo pgm con LR
009900060207     c                   move      '  ?'         d01mtv
010000060207     c                   clear                   d01dmt
010100060207     c                   clear                   d01nt1
010200060207     c                   clear                   d01nt2
010300060207     c                   endsr
010400060209     C*****************************************************************
010500060209     c     gesvid        begsr
010600060209     C*****************************************************************
010700060209     c* con la causale ricevuta chaino tabella "7L" per verificare
010800060209     c* se richiesto il motivo variazione
010900060209     c                   movel(P)  ilv85cvb      kkey
011000060209     c                   clear                   ds7l
011100060209     c     ktab          chain     tabel00f
011200060209     c                   if        %found(tabel00f)
011300060209     c                   movel     tbluni        ds7l
011400060209     c                   endif
011500060209     C*
011600060209    2c                   if        §7lmtv='S'
011700060209     C* EMISSIONE FORMATO
011800060209     C                   WRITE     LV85D02
011900060209     C     FMT01         TAG
012000060209     C                   EXFMT     LV85D01
012100060209     c                   setoff                                       9028
012200060209     c                   clear                   d01msg
012300060209     C*
012400060209     C** CMD12 - FINE LAVORO
012500060209     c   kl              eval      olv85f12 = '1'
012600060209     C   KL              GOTO      Fgesvid
012700060209     C*
012800060209     c                   exsr      ctrd01
012900060209     c   90
013000060209     cornkf              goto      fmt01
013100060209     c*
013200060209     C     fgesvid       TAG
013300060209     c                   movel     d01mtv        olv85mtv
013400060209     c                   movel     d01nt1        olv85nt1
013500060209     c                   movel     d01nt2        olv85nt2
013600060209     C*
013700060209    2c                   endif
013800060209     c                   endsr
013900060207     C*****************************************************************
014000060207     c     ctrd01        begsr
014100060207     C*****************************************************************
014200060207     c* codice motivo variazione
014300060207     c                   clear                   d01dmt
014400060207     C* RICERCA
014500060207     C     '?'           SCAN      d01mtv                                 30
014600060207If  2c                   If        *In30
014700060207     c                   Clear                   tibs02ds
014800060207     c                   Eval      T02Mod = 'R'
014900060207     c                   Eval      T02Sif = Knsif
015000060207     c                   Eval      T02Cod = 'MTV'
015100060207     c                   Call      'TIBS02R'
015200060207     c                   Parm                    Kpjba
015300060207     c                   Parm                    tibs02ds
015400060207     c                   Movel     T02Ke1        d01mtv
015500060207     c                   Movel     T02uni        dmtv
015600060207     c                   movel     §mtvdes       d01dmt
015700060207     c                   seton                                        0190
015800060207     C                   WRITE     LV85D02
015900060207     c                   GoTo      ENDc01
016000060207    1C                   ENDIF
016100060207     c* validità
016200060207     c                   if        d01mtv = *blanks
016300060207     c                   seton                                        019028
016400060207     c                   movel     msg(1)        d01msg
016500060207     c                   goto      endc01
016600060207     c                   endif
016700060207     C                   CLEAR                   tibs02ds
016800060207     c                   clear                   dmtv
016900060207     C                   MOVEL     'C'           T02MOD
017000060207     C                   MOVEL     KNSIF         T02SIF
017100060207     C                   MOVEL     'MTV'         T02COD
017200060207     c                   movel(P)  d01mtv        t02ke1
017300060207     C                   CALL      'TIBS02R'
017400060207     C                   PARM                    KPJBA
017500060207     C                   PARM                    tibs02ds
017600060207    2C                   IF        T02ERR = *BLANKS
017700060207     C                   MOVEL     T02uni        dmtv
017800060207     c                   movel     §mtvdes       d01dmt
017900060207     c                   else
018000060207     c                   seton                                        019028
018100060207     c                   movel     msg(1)        d01msg
018200060207     c                   goto      endc01
018300060207    2C                   ENDIF
018400060207     c* note se obbligatorie
018500060207     c                   if        §mtvnob='S' and
018600060207     c                             d01nt1 = *blanks  and d01nt2=*blanks
018700060207     c                   seton                                        029028
018800060207     c                   movel     msg(2)        d01msg
018900060207     c                   goto      endc01
019000060207     c                   endif
019100060207     c
019200060207     c     endc01        tag
019300060207     c                   endsr
019400060209     C*****************************************************************
019500060209     c     geswrt        begsr
019600060209     C*****************************************************************
019700060209     c                   clear                   fiar5000
019800060209     c                   z-add     ilv85aas      ar5aas
019900060209     c                   z-add     ilv85lnp      ar5lnp
020000060209     c                   z-add     ilv85nrs      ar5nrs
020100060209     c                   z-add     ilv85nsp      ar5nsp
020200060209     c                   eval      ar5trd = '§' + ilv85cvb
020300060209     c                   if        ilv85dac <> *zeros
020400060209     c                   z-add     ilv85dac      ar5dac
020500060209     c                   z-add     ilv85orc      ar5orc
020600060209     c                   else
020700060209     c                   z-add     dateu         ar5dac
020800060209     c                   time                    ar5orc
020900060209     c                   endif
021000060209     c                   clear                   dar5mtv
021100060209     c                   if        ilv85mtv <> *blanks
021200060209     c                   eval      §ar5mtv = ilv85mtv
021300060209     c                   eval      §ar5txt = ilv85nt1 + ilv85nt2
021400060209     c                   else
021500060209     c                   eval      §ar5mtv = d01mtv
021600060209     c                   eval      §ar5txt = d01nt1 + d01nt2
021700060209     c                   endif
021800060209     c                   movel     dar5mtv       ar5uni
021900060209     c*
022000060209     c                   write     fiar5000
022100060209     c
022200060209     c                   endsr
022300060207**
022400060207Codice mancante o errato
022500060207Immettere descrizione del motivo della variazione
