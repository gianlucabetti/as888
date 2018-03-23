000100980127     H DECEDIT('0,') DATEDIT(*YMD.)
000200981106      *---------------------------------------------------------------------*
000300031031     FTITAS30C  IF   E           K DISK
000400031031     F******fiapd01l  IF   E           K DISK
000500031031     FFIFCE03L  UF   E           K DISK
000600031031     Ffifcesav  O    E             DISK    rename(FIFCE000:FCESAV)
000700031031     FTNVRA99D  CF   E             WORKSTN usropn
000800980127      *---------------------------------------------------------------------*
000900980128      * DS
001000980128      *---------------------------------------------------------------------*
001100031031     D DateDS          DS
001200031031     D  Da8a                   1      8
001300031031     D  A8a                    9     16
001400980127      *  Ds per inversione data
001500961003     D WLBDA8          DS
001600980127     D  G08DAT                 1      8  0
001700980127     D  G08INV                 9     16  0
001800980127     D  G08ERR                17     17
001900980127     D  G08TGI                18     22  0
002000961003     D                SDS
002100961003     D  NOMPGM                 1     10
002200031031      *------------------------------------------------------------------------*
002300031031     ITITAS000      01
002400031031     ITITAS010      02
002500031031     ITITASP00      03
002600981106      *---------------------------------------------------------------------*
002700980127      * Ciclo principale
002800980127      *---------------------------------------------------------------------*
002900031031     c                   if        dateds = *blank
003000031031     c                   open      tnvra99D
003100031031     c     inizio        tag
003200031031     c                   clear                   vid1
003300031031     c     video1        tag
003400031031     c                   exfmt     vid1
003500031031     c                   exsr      ctr1
003600031031     c   kc              goto      fine
003700031031     c  nkf
003800031031     cor 99              goto      video1
003900031031     c                   exfmt     vid2
004000031031     c   kc              goto      fine
004100031031     c   kL              goto      Video1
004200031031      *
004300031031     c                   else
004400031031     c                   move      Da8a          datakey1
004500031031     c                   move      A8a           datakey2
004600031031     c                   end
004700031031     c                   exsr      elabora
004800961003      * Fine Lavoro
004900961003     C     FINE          TAG
005000980128     C                   EVAL      *INLR = *ON
005100980127      *---------------------------------------------------------------*
005200031031      *  ctr1   Inizializzo prima videata
005300980127      *---------------------------------------------------------------*
005400031031     C     ctr1          BEGSR
005500031031      *
005600031031     c     d8dmg         mult      10000         dadata            8 0
005700031031     c                   add       d8anno        dadata
005800031031     c     d8amg         mult      10000         adata             8 0
005900031031     c                   add       d8anno        adata
006000031031      *
006100031031      *  Controllo le date
006200031031     C                   Z-ADD     dadata        G08DAT
006300031031     C                   MOVEL     *BLANKS       G08ERR
006400031031     C                   CALL      'XSRDA8'
006500031031     C                   PARM                    WLBDA8
006600031031     C                   IF        G08ERR = '1'
006700031031     C                   seton                                        5099
006800031031     C                   GOTO      FINCT1
006900031031     C                   ELSE
007000031031     C                   Z-ADD     G08INV        dataKey1          8 0
007100031031     C                   Z-ADD     dadata        indat1
007200031031     C                   END
007300031031      *
007400031031      *  Controllo le date
007500031031     C                   Z-ADD     adata         G08DAT
007600031031     C                   MOVEL     *BLANKS       G08ERR
007700031031     C                   CALL      'XSRDA8'
007800031031     C                   PARM                    WLBDA8
007900031031     C                   IF        G08ERR = '1'
008000031031     C                   seton                                        5199
008100031031     C                   GOTO      FINCT1
008200031031     C                   ELSE
008300031031     C                   Z-ADD     G08INV        dataKey2          8 0
008400031031     C                   Z-ADD     adata         indat2
008500031031     C                   END
008600031031      *
008700031031     C                   IF        dataKey2 < dataKey1
008800031031     C                   seton                                        5299
008900031031     C                   GOTO      FINCT1
009000031031     C                   END
009100980127      *
009200031031     C     finct1        ENDSR
009300980127      *----------------------------------------------------*
009400031031      *
009500980127      *----------------------------------------------------*
009600031031     C     elabora       BEGSR
009700980127      *
009800031031     c     datakey1      setll     fifce03l
009900031031     c                   read      fifce03l
010000031031      *
010100031031     c                   dow       not %eof(fifce03l) and
010200031031     c                             fceddc <= datakey2
010300031031     c                   if        fcetsr = 'C'
010400031031      *
010500031031     C     ktas          chain     titas30C
010600031031      *
010700031031     c                   if        %found(titas30C)  and
010800031031     c                             fcefgs <> taslna
010900031031     c*****                                 and fceice=0
011000031031      *****
011100031031     C*****kapd          chain     fiapd01l
011200031031     c*****              if        %found(fiapd01l) and apdpdd ='S'
011300031031      *
011400031031     c                   delete    fifce000
011500031031     c                   write     fcesav
011600031031      *
011700031031     c*****              end
011800031031     c                   end
011900031031     c                   end
012000031031      *
012100031031     c                   read      fifce03l
012200031031     C                   ENDdo
012300031031      *
012400031031     C                   ENDSR
012500031031      *----------------------------------------------------*
012600031031      *   *INZSR: Operazioni iniziali                      *
012700031031      *----------------------------------------------------*
012800031031     C     *INZSR        BEGSR
012900031031      *
013000961003     C     *ENTRY        PLIST
013100031031     C                   PARM                    DAteds
013200980127      *
013300031031     C     ktas          klist
013400031031     C                   kfld                    fceAAS
013500031031     C                   kfld                    fceLNP
013600031031     C                   kfld                    fceNRS
013700031031     C                   kfld                    fceNSP
013800031031      *
013900031031     C     kapd          klist
014000031031     C                   kfld                    tipo
014100031031     C                   kfld                    fcePDR
014200031031     C                   move      'A'           tipo              1
014300031031      *
014400031031     C                   ENDSR
014500031031      *----------------------------------------------------*
