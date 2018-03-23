000100970910     H*------------------------------------------------------------------------*
000200110504     H*   Anagrafica punti di scambio traini  - filtro stampa
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000800110504     FTNTLP2D   CF   E             WORKSTN
000900060421     d
001000060421     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
001100970910     D*------------
001200970910     D* DS PARAMETRI DI LANCIO
001300970910     D*------------
001400970610     D PARAM           DS                  INZ
001500110504     D  PARdri                 1      8  0
001600110504     D  PARdrigma              9     16  0
001700110504     D  PARord                17     17
001800060421     d
001900970910     D KPJBA         E DS
002000060421     d Azuteds       e ds                  extname(Azute00f)
002100060421     d dDatiute      e ds
002200060421     D TIBS34DS      E DS
002300110504     d Dataiso         s               d   datfmt(*iso)
002400110504     d Dataeur         s               d   datfmt(*eur)
002500110504     d amgdat          s              8  0
002600110504     d amgdri          s              8  0
002700110504     d wdat            s              8  0
002800060421     d
002900970910     C*------------------------------------------------------------------------*
003000970910     C*
003100060420     c* Impostazioni iniziali
003200110504     c                   clear                   v1cdri
003300110504     c                   eval      v1cord='S'
003400060421     C* EMETTE IL VIDEO
003500060421     c     for01         tag
003600110504     C                   EXFMT     tlP2D01
003700060421     c                   setoff                                       2890
003800110504     c                   setoff                                       41
003900060421     c
004000060421     c* F3 - Fine
004100060421     c   kc              goto      fine
004200970910     C*
004300060421     c* controlli
004400060421     c                   EXSR      CTRD01
004500060421     c  nkf
004600060421     cor 90              goto      for01
004700060421     c
004800060421     c* F6 - Elaborazione
004900060421     c                   if        *inkf
005000110504     c                   movel     v1cdri        pardrigma
005100110504     c                   movel     amgdri        pardri
005200110504     c                   movel     v1cord        parord
005300060421     C                   MOVEL     PARAM         KPJBU
005400110504     C                   MOVEL     'TLP3'        KCOAZ
005500060421     C                   CALL      'BCH10'
005600060421     C                   PARM                    KPJBA
005700060421     c                   endif
005800970910     C* FINE PGM
005900060421     c     fine          tag
006000970910     C                   SETON                                        LR
006100970910     C*------------------------------------------------------------------------*
006200060421     C* CONTROLLA CAMPI E DECODIFICA
006300970910     C*------------------------------------------------------------------------*
006400060421     C     CTRD01        BEGSR
006500110504     c                   clear                   amgdat
006600110504     c                   clear                   amgdri
006700110504     c
006800110504     c                   if        v1cdri>0
006900110504     c                   eval      wdat=v1cdri
007000110504     c                   exsr      CTRDEV
007100110504     c                   eval      v1cdri=wdat
007200110504
007300110504     c                   if        *in90
007400110504     c                   seton                                        41
007500110504     C                   MOVEL     MSG(1)        V1cMSG
007600110504     c                   goto      endct1
007700110504    1c                   endif
007800110504     c                   eval      amgdri=amgdat
007900110504     c                   endif
008000970910     C*
008100060421     C     endct1        ENDSR
008200110504      *-----------------------------------------------------***********
008300110504      * CONTROLLo Data
008400110504      *-----------------------------------------------------***********
008500110504     c     CTRDEV        BEGSR
008600110504    1c                   if        wdat>0
008700110504     c                   movel     wdat          w002a             2
008800110504     c     *eur          test(d)                 wdat                   30
008900110504    2c                   if        *in30  and w002a='00'
009000110504     c     *dmy          test(d)                 wdat                   30
009100110504     c                   endif
009200110504     c* Errore
009300110504    3c                   if        *in30
009400110504     c                   seton                                        9028
009500110504     c                   goto      endctdev
009600110504     c                   endif
009700110504     c
009800110504     c                   if        w002a='00'
009900110504     c     *dmy          move      wdat          dataeur
010000110504     c                   else
010100110504     c     *eur          move      wdat          dataeur
010200110504     c                   endif
010300110504     c
010400110504     c                   else
010500110504     c                   seton                                        9028
010600110504     c                   goto      endctdev
010700110504     c                   endif
010800110504     c* la trasformo in AAAAAMMGG - iso
010900110504     c                   move      dataeur       wdat
011000110504     c                   movel     dataeur       dataiso
011100110504     c     *iso          movel     dataiso       amgdat            8 0
011200110504     c
011300110504     c
011400110504     c     endctdev      ENDSR
011500970910     C*------------------------------------------------------------------------*
011600970910     C* *INZSR - OPERAZIONI INIZIALI
011700970910     C*------------------------------------------------------------------------*
011800970910     C     *INZSR        BEGSR
011900970910     C*---
012000970910     C* RICEVIMENTO PARAMETRI
012100970910     C*---
012200970910     C     *ENTRY        PLIST
012300970612     C                   PARM                    KPJBA
012400060421     c
012500110504     c                   z-add     1             codut             1 0
012600060421      *
012700060421     c     *dtaara       define    §azute        azuteds
012800060421     c     *dtaara       define    §datiute      ddatiute
012900060421     c                   in(E)     *dtaara
013000060421    1c                   If        %error  or RSUT = *blanks
013100060421     c                   Clear                   Tibs34ds
013200060421     c                   Call      'TIBS34R'
013300060421     c                   Parm                    Tibs34ds
013400060421     c                   In        *dtaara
013500060421    1c                   EndIf
013600060421    cc                   movel     rsut          v1crsu
013700060421     c                   ENDSR
013800970910     O*------------------------------------------------------------------------*
013900970910** MSG - MESSAGGI
014000110504Data di riferimento errata
