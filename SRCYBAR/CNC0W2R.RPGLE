000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020926     H OPTION(*NODEBUGIO)
000300900606?     *--------------------------------------------------------------*
000400021126      * CNC0W2R  annullamento quadratura incassi giornalieri         *
000500991029?     *--------------------------------------------------------------*
000600910521      * 03 -FINE LAVORO                                              *
000700910521      * 06 -AGGIORNAMENTO                                            *
000800900606?     *--------------------------------------------------------------*
000900021126     FCNC0W2d   CF   E             WORKSTN
001000030318     Fazorg01l  iF   E           k disk
001100060224     Fyfqig02L  uF   E           K DISK
001200060224     Fyfqig01L  iF   E           K DISK    rename(yfqig000:yfqig001)
001300991029?     *--------------------------------------------------------------*
001400021126     D kpjba         e ds
001500030404     D msg             S             60    DIM(3) CTDATA PERRCD(1)              SCHIERA ER
001600021126     D savdta          S                   like(qigdta)                         DS3A TIPO
001700060224     D savimpc         S                   like(qigicon)                        DS3A TIPO
001800060224     D savimpa         S                   like(qigiass)                        DS3A TIPO
001900021126     D dtaiso          S               d   inz                                  DS3A TIPO
002000021126     D dtaeur          S               d   datfmt(*eur) inz                     DS3A TIPO
002100991029?     *--------------------------------------------------------------*
002200991029      * CICLO PRINCIPALE
002300991029?     *--------------------------------------------------------------*
002400030404     c* controllo password
002500030404     c                   do        *hival
002600030404     C                   EXFMT     CN0W101
002700030404     c                   clear                   $msg
002800030404     c   kc              leave
002900030411     c                   if        passv = 'ONELIO'
003000021115     c                   do        *hival
003100021126     C                   EXFMT     CN0W102
003200021125     C   kc              leave
003300030318     c* controlli
003400030318     c                   exsr      ctr01
003500030318     c   20              iter
003600021126     c                   if        *inkf and dtav <> 0
003700021115     c                   exsr      aggio
003800021115     c                   leave
003900021115     c                   end
004000991102     C                   ENDDO
004100030404     c                   else
004200030404     c                   movel     msg(3)        $msg
004300030404     C                   END
004400030404     c                   if        *inkc or *inkf
004500030404     c                   leave
004600030404     C                   END
004700030404     C                   ENDDO
004800991029     C*
004900910521     C                   SETON                                        LR
005000021115     C**-------------------------------------------------------------
005100030318     C** controlli
005200021115     C**-------------------------------------------------------------
005300030318     C     ctr01         BEGSR
005400021126     c                   setoff                                       20
005500021126     c                   clear                   $msg
005600030506     c                   clear                   SAVDTA
005700060224     c                   clear                   SAVIMPC
005800060224     c                   clear                   SAVIMPA
005900030318     c* controllo po
006000030318     c     pov           chain     azorg01l
006100030318     c                   if        not %found(azorg01l)
006200030318     c                   seton                                        2001
006300030318     c                   else
006400030318     c*
006500021126     c                   move      amgudt        qigdta
006600021126     c*
006700060224     c     kqig          setgt     yfqig02l
006800021126     c                   do        *hival
006900060224     c     pov           readpe    yfqig02l
007000060224     c                   if        %eof(yfqig02l)
007100021126     c                   if        savdta = 0
007200021126     c                   seton                                        20
007300021126     c                   movel     msg(2)        $msg
007400021126     c                   end
007500021126     c                   leave
007600021126     c                   else
007700021126     c* memorizzo i dati del primo record non annullato. Se il primo
007800021126     c* record è annullato emmetto l'errore
007900021126     c                   if        savdta = 0
008000021126     c                   if        qigann = ' '
008100021126     c                   move      qigdta        savdta
008200060224     c                   move      qigicon       savimpc
008300060224     c                   move      qigiass       savimpa
008400021126     c                   iter
008500021126     c                   else
008600021126     c                   seton                                        20
008700021126     c                   movel     msg(1)        $msg
008800021126     c                   end
008900021126     c                   else
009000021126     c* se il secondo record è annullato controllo che esista anche il
009100021126     c* corrispondente valido. Se non esiste do errore
009200021126     c                   if        qigann = 'A'
009300021126     c                   clear                   qigann
009400060224     c     kqig2         chain     yfqig01l
009500060224     c                   if        not %found(yfqig01l)
009600021126     c                   seton                                        20
009700021126     c                   movel     msg(1)        $msg
009800021126     c                   end
009900021126     c                   end
010000021126     c                   end
010100021126     c                   leave
010200021126     c                   end
010300021126     c                   enddo
010400030318     c                   end
010500021126     c*
010600021126     c                   if        not *in20
010700021126     c                   move      savdta        dtaiso
010800021126     c                   move      dtaiso        dtaeur
010900021126     c                   move      dtaeur        dtav
011000060224     c                   z-add     savimpc       impcv
011100060224     c                   z-add     savimpa       impav
011200021126     c                   else
011300021126     c                   z-add     0             dtav
011400060224     c                   z-add     0             impcv
011500060224     c                   z-add     0             impav
011600021126     c                   end
011700021115     c                   endsr
011800021120     C**-------------------------------------------------------------
011900021120     C** aggiorna
012000021120     C**-------------------------------------------------------------
012100021120     C     aggio         BEGSR
012200021126     c                   clear                   qigann
012300060224     c     kqig1         chain     yfqig02l
012400060224     c                   if        %found(yfqig02l)
012500021126     c                   move      'A'           qigann
012600060224     c                   update    yfqig000
012700021126     c                   end
012800021120     c                   endsr
012900991102     C*-----------------------------------------------------------*
013000991029     C*  OPERAZIONI INIZIALI
013100991029     C*-----------------------------------------------------------*
013200941124     C     *INZSR        BEGSR
013300991029     C*
013400000000     C     *ENTRY        PLIST
013500000000     C                   PARM                    KPJBA
013600030318     c*
013700021126     C                   move      UDATE         dtaeur
013800021126     C                   move      dtaeur        dtaiso
013900021126     C                   move      dtaiso        AMGUDT            8 0
014000900515      *---------------------------------------------------------------*
014100900515      * CHIAVI                                                        *
014200900515      *---------------------------------------------------------------*
014300021119     C     Kqig          KLIST
014400030318     C                   KFLD                    pov
014500021126     C                   KFLD                    qigdta
014600021126     C     Kqig1         KLIST
014700030318     C                   KFLD                    pov
014800021126     C                   KFLD                    savdta
014900021126     C                   KFLD                    qigann
015000021126     C     Kqig2         KLIST
015100021126     C                   KFLD                    qigann
015200030318     C                   KFLD                    pov
015300021126     C                   KFLD                    qigdta
015400991102     C*
015500991102     C                   ENDSR
015600021126** MSG
015700021126Non è possibile eseguire l'annullamento. Ne esiste già uno!!
015800021126Non esiste nulla da annullare.
015900030404Password non corretta
