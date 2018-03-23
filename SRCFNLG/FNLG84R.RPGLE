000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200941012     F*--------------------------------------------------------------*
000300941012     F*    FNLG84R
000400941012     F*--------------------------------------------------------------*
000500050218     Ftigcp24L  IF   E           K DISK
000600050322     Ftigcp02L  UF   E           K DISK    commit
000700050218     F                                     RENAME(tIGCP000:tIGCP2)
000800050218     Ffigic00f  o    E             DISK
000900941012     FFNARB01L  IF   E           K DISK
000901050411     d trul06ds      e ds
000902050411     d  lin                    1     90  0 dim(30)
000903050411     d savkpjbu        s                   like(kpjbu)
001000941012     D*--------------------------------------------------------------*
001100900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001200900418     D KPJBA         E DS
001300020807     D CNCR80        E DS
001301171003      *ds per richiamo monitor variazioni
001302171003     d tigcp         e ds                  extname(tigcp00f)
001303171003     d fnlg33ds      e ds                  inz
001304171003     d tigcpa        e ds                  extname(tigcp00f)
001305171003     d                                     prefix(a)
001306171003     d tigcpb        e ds                  extname(tigcp00f)
001307171003     d                                     prefix(b)
001400941012     D WLBDA8          DS
001500941012     D  G02DAT                 1      8  0
001600941012     D  G02INV                 9     16  0
001700941012     D  G02ERR                17     17
001800941012     D  G02TGI                18     22  0
001801050411     D tibs56ds      E DS
001802050411     d  lix                    7    756  0 dim(250)
002200911017     C*---------------------------------------------------------------*
002300050411     c                   do        250           f                 3 0
002400020808     c                   setoff                                       20
002500050411     c                   if        lix(f) = 0
002600020807     c                   leave
002700020807     c                   endif
002701050411     c                   if        lix(f) = 046
002702050411     c                   iter
002703050411     c                   endif
002704050411     C* Caricamento £1
002705050411     c                   clear                   trul06ds
002706050411     c                   movel     kpjbu         savkpjbu
002707050411     c                   eval      d06cod = 'PP'
002708050411     c                   movel     lix(f)        d06key
002709050411     c                   movel     trul06ds      kpjbu
002710050411     c                   call      'TRUL06R'
002711050411     c                   parm                    kpjba
002712050411     c                   movel     kpjbu         trul06ds
002713050411     c                   movel     savkpjbu      kpjbu
002714050411     c                   do        30            ff                3 0
002715050411     c                   setoff                                       20
002716050411     c                   if        lin(ff) = 0
002717050411     c                   leave
002718050411     c                   endif
002800050218     C     lin(ff)       SETLL     tigcp24l
002900010606     C                   DO        *HIVAL
003000050218     C     lin(ff)       READe     tigcp24l                               20
003100010606     C   20              LEAVE
003200050218     C* CONSIDERO LE GIAC. CON FASE = 35 E PROGRESSIVO 0
003300050218     C     GCPFAS        ifeq      35
003400941012     C     GCPFRG        ANDEQ     0
003500050218     C*
003600941012     C     KARB          CHAIN     FNARB01L                           18
003700050218     C                   if        *in18
003800050218     c                   iter
003900050218     c                   end
004000050218     c* alloco la giacenza
004100050218     C     KGCA          CHAIN     tigcp2                             2221
004200050218     c                   if        *in21 or *in22
004300050218     C                   ITER
004400050218     c                   end
004500050218     C* CHIUDO GIACENZA SE MOTIVO CHIUSURA E' L'APERTURA DI UNA PRATICA C.A.
004600050218     c* oppure se consegnata (fase 40 o 50 se disposizioni di RESO)
004700050218     C     GCPCFG        ifeq      '015'
004800050218     C     ARBDCM        orne      0
004900050218     c                   exsr      sraggio
005000050322     c                   commit
005100050218     C                   END
005200050218     C                   END
005300020807     C                   enddo
005400050218     C                   enddo
005401050411     C                   enddo
005500900418     C                   SETON                                        LR
005600941012     C*-------------------------------------*
005700050218     C* aggiorno la giacenza
005800941012     C*-------------------------------------*
005900050218     C     sraggio       BEGSR
005901171003     c                   eval      tigcpb = tigcp
006000050218     c                   if        gcpdis = 'R'
006100050218     C                   Z-ADD     50            GCPFAS
006200050218     C                   Z-ADD     DATEU         GCPDBG
006300050218     c                   else
006400050218     C                   Z-ADD     40            GCPFAS
006500050218     c                   clear                   gcpdbg
006600050218     c                   end
006700050218     C                   Z-ADD     DATEU         GCPDCG
006800050218     C                   EXCEPT    AGG
006802171003     c                   exsr      RegVariazioni
006900050218     c* scrivo figic00f
007000050218     c                   exsr      gicwtr
007100050218     c                   endsr
007101171003     C*__________________________________________________________
007102171003     c     RegVariazioni begsr
007103171003     C*__________________________________________________________
007104171003     c                   eval      ILG33CTA = 'M'
007105171003     c                   eval      ILG33PRU = knmus
007106171003     c                   eval      ILG33NOJ = knmeb
007107171003     c                   eval      ILG33PGM = 'FNLG84R'
007108171003     c                   eval      tigcpa = tigcp
007109171003     c                   call      'FNLG33R'
007110171003     c                   parm                    fnlg33ds
007111171003     c                   parm                    tigcpb
007112171003     c                   parm                    tigcpa
007113171003     c                   endsr
007200050218     C*---------------------------------------------------------------*
007300050218     C* Scrivo figic00f per allineare saltuariamente la sede (TITAS+TNCSB)
007400050218     C*---------------------------------------------------------------*
007500050218     C     gicwtr        BEGSR
007600050218     C                   MOVEL     'CHGC'        GICCAV
007700050218     C                   Z-ADD     DATEU         GICDTV
007800050218     C                   TIME                    GICORV
007900050411     C                   Z-ADD     GCPLNPa       GICFEV
008000050411     C                   Z-ADD     GCPLNPa       GICLNP
008100050411     C                   Z-ADD     GCPLNAa       GICLNA
008200050411     C                   Z-ADD     GCPAASa       GICAAS
008300050411     C                   Z-ADD     GCPNRSa       GICNRS
008400050411     C                   Z-ADD     GCPNSPa       GICNSP
008500050218     C                   MOVEL     GCPAGC        GICDGC
008600050218     C                   MOVE      GCPMGC        GICDGC
008700050218     C                   Z-ADD     GCPFGC        GICFGC
008800050218     C                   Z-ADD     GCPNGC        GICNGC
008900050218     C                   Z-ADD     ARBDCM        GICDCH
009000050218     C                   WRITE     FIGIC000
009100050218     C                   ENDSR
009200050218     C*-------------------------------------*
009300050218     C*   OPERAZIONI INIZIALI
009400050218     C*-------------------------------------*
009500050218     C     *INZSR        BEGSR
009600900418     C     *ENTRY        PLIST
009700900418     C                   PARM                    KPJBA
009800900418     C                   Z-ADD     1             CODUT
009900900418     C                   CALL      'X§PARUT'
010000900418     C                   PARM                    UTEDSE
010100900418     C                   MOVEL     RAGUT         RSUT             20
010200900418     C                   MOVEL     RAGUT         RSUT2            25
010300900502     C                   MOVEL     RAGUT         PARDUT           30
010400020807     C                   MOVEL     REC80         CNCR80
010500911010     C*
010600911106     C     KARB          KLIST
010700050218     C                   KFLD                    GCPAASa
010800050218     C                   KFLD                    GCPLNPa
010900050218     C                   KFLD                    GCPNRSa
011000050218     C                   KFLD                    GCPNSPa
011100010606      *
011200010606     C     KGCA          KLIST
011300010606     C                   KFLD                    GCPAGC
011400010606     C                   KFLD                    GCPFGC
011500010606     C                   KFLD                    GCPNGC
011600010606     C                   KFLD                    GCPFRG
011700941012     C* Giro la data del giorno
011800941012     C                   TIME                    WHHDAT           14 0
011900941012     C                   MOVE      WHHDAT        G02DAT
012000941012     C                   MOVE      *ZEROS        G02INV
012100950103     C                   MOVEL     ' '           G02ERR
012200950103     C                   CALL      'XSRDA8'
012300941012     C                   PARM                    WLBDA8
012400941012     C                   Z-ADD     G02INV        DATEU             8 0
012401050411     C* CARICO I SIMFEL dell'as
012402050411     c                   clear                   tibs56ds
012403050411     c                   eval      i56ppo=046
012404050411     c                   eval      i56mod='AZI'
012405050411     c                   call      'TIBS56R'
012406050411     c                   parm                    tibs56ds
013500941012     C*
013600900418     C                   ENDSR
013700911106      *
013800050218     OTIGCP2    E            AGG
013900911211     O                       GCPFAS
014000911211     O                       GCPDCG
014100050218     O                       GCPDBG
