000100970910     H*------------------------------------------------------------------------*
000200060420     H*   Eventi Particolari  - STAMPA   - RICHIESTA PARAMETRI
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000800060421     FTISIP2D   CF   E             WORKSTN
000900060421     Ftabel00f  IF   E           K DISK
001000060421     d
001100060421     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
001200970910     D*------------
001300970910     D* DS PARAMETRI DI LANCIO
001400970910     D*------------
001500970610     D PARAM           DS                  INZ
001600970910     D  PARTOR                 1      1
001700060421     D  PARann                 2      5  0
001800060421     D  PARCEV                 6      8
001900060421     d
002000970910     D KPJBA         E DS
002100060421     D DS2Z          E DS
002200060421     d Azuteds       e ds                  extname(Azute00f)
002300060421     d dDatiute      e ds
002400060421     D TIBS34DS      E DS
002500060421     d
002600060421     d kcod            s                   Like(tblCod)
002700060421     d kkey            s                   Like(TblKey)
002800970910     C*------------------------------------------------------------------------*
002900970910     C*
003000060420     c* Impostazioni iniziali
003100060421     c                   eval      v1ctor='D'
003200060421     c                   clear                   v1ccev
003300060421     c                   clear                   v1dcev
003400060421     c                   clear                   v1cann
003500060421     C* EMETTE IL VIDEO
003600060421     c     for01         tag
003700060421     C                   EXFMT     SIP2D01
003800060421     c                   setoff                                       2890
003900060421     c                   setoff                                       4142
004000060421     c
004100060421     c* F3 - Fine
004200060421     c   kc              goto      fine
004300970910     C*
004400060421     c* controlli
004500060421     c                   EXSR      CTRD01
004600060421     c  nkf
004700060421     cor 90              goto      for01
004800060421     c
004900060421     c* F6 - Elaborazione
005000060421     c                   if        *inkf
005100060421     c                   movel     v1ctor        partor
005200060421     c                   movel     v1cann        parann
005300060421     c                   movel     v1ccev        parcev
005400060421     C                   MOVEL     PARAM         KPJBU
005500060421     C                   MOVEL     'SIP3'        KCOAZ
005600060421     C                   CALL      'BCH10'
005700060421     C                   PARM                    KPJBA
005800060421     c                   endif
005900970910     C* FINE PGM
006000060421     c     fine          tag
006100970910     C                   SETON                                        LR
006200970910     C*------------------------------------------------------------------------*
006300060421     C* CONTROLLA CAMPI E DECODIFICA
006400970910     C*------------------------------------------------------------------------*
006500060421     C     CTRD01        BEGSR
006600060421     c* Anno
006700060421     C*--------------------
006800060421     c                   if        v1cann>0
006900060421     c* deve essere >=2000
007000060421     c                   if        v1cann<2000
007100060421     c                   seton                                        419028
007200060421     C                   MOVEL     MSG(2)        V1cMSG
007300060421     c                   goto      endct1
007400060421     c                   endif
007500060421     c                   endif
007600060421     c
007700060420     C* codice Evento
007800970910     C*--------------------
007900060421     c                   exsr      ctrcev
008000060421     c   90              goto      endct1
008100060421     c
008200970910     C*
008300060421     C     endct1        ENDSR
008400060420      *-----------------------------------------------------***********
008500060420      * CONTROLLo codice Evento
008600060420      *-----------------------------------------------------***********
008700060420     c     CTRcev        BEGSR
008800060420     c                   clear                   v1dcev
008900060420      * codice evento
009000060421     c     '?'           scan      v1ccev                                 90
009100060420    1c                   if        *in90
009200060420     C                   movel     '2A'          cod
009300060420     C                   CLEAR                   §KEY
009400060420     C                   CLEAR                   §des
009500060420     C                   CALL      'X§TABER'
009600060420     C                   PARM                    CODUT
009700060420     C                   PARM                    COD               2
009800060420     C                   PARM                    §KEY              8
009900060420     C                   PARM                    §DES             25
010000060421     c                   movel     §key          v1ccev
010100060420     c                   movel     §des          v1dcev
010200060420     c                   goto      endctcev
010300060420    1c                   endif
010400060420     c
010500060421     c                   if        v1ccev<>*blanks
010600060420     c                   eval      kcod='2Z'
010700060421     c                   movel(p)  v1ccev        kkey
010800060420     c     ktab          chain     tabel00f
010900060420    1c                   if        not %found(tabel00f) or tblflg<>' '
011000060420     c                   seton                                        409028
011100060421     C                   MOVEL     MSG(1)        V1cMSG
011200060420     c                   goto      endctcev
011300060420   x1c                   else
011400060420     c                   movel     tbluni        ds2z
011500060420    2c                   if        §2zcep=' '
011600060420     c                   seton                                        409028
011700060421     C                   MOVEL     MSG(1)        V1cMSG
011800060420     c                   goto      endctcev
011900060420    2C                   ENDIF
012000060420     c
012100060420     c                   eval      kcod='2A'
012200060421     c                   movel(p)  v1ccev        kkey
012300060420     c     ktab          chain     tabel00f
012400060420    2c                   if        not %found(tabel00f) or tblflg<>' '
012500060420     c                   seton                                        409028
012600060421     C                   MOVEL     MSG(1)        V1cMSG
012700060420     c                   goto      endctcev
012800060420   x2c                   else
012900060420     c                   movel     tbluni        v1dcev
013000060420     c
013100060420    2c                   endif
013200060420    1c                   endif
013300060421    1c                   endif
013400060420     c     endctcev      ENDSR
013500970910     C*------------------------------------------------------------------------*
013600970910     C* *INZSR - OPERAZIONI INIZIALI
013700970910     C*------------------------------------------------------------------------*
013800970910     C     *INZSR        BEGSR
013900970910     C*---
014000970910     C* RICEVIMENTO PARAMETRI
014100970910     C*---
014200970910     C     *ENTRY        PLIST
014300970612     C                   PARM                    KPJBA
014400060421     c
014500060421     c     Ktab          Klist
014600060421     c                   Kfld                    codut             1 0
014700060421     c                   Kfld                    kCod
014800060421     c                   Kfld                    kKey
014900060421     c
015000060421     c                   z-add     1             codut
015100060421      *
015200060421     c     *dtaara       define    §azute        azuteds
015300060421     c     *dtaara       define    §datiute      ddatiute
015400060421     c                   in(E)     *dtaara
015500060421    1c                   If        %error  or RSUT = *blanks
015600060421     c                   Clear                   Tibs34ds
015700060421     c                   Call      'TIBS34R'
015800060421     c                   Parm                    Tibs34ds
015900060421     c                   In        *dtaara
016000060421    1c                   EndIf
016100060421    cc                   movel     rsut          v1crsu
016200060421     c                   ENDSR
016300970910     O*------------------------------------------------------------------------*
016400970910** MSG - MESSAGGI
016500060421Codice Evento errato o non utilizzabile nel calendario                         02
016600060421Anno errato                                                                    02
