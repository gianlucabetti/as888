000100921203     H DECEDIT('0,') DATEDIT(*DMY.)
002800050412     FFNarb01l  iF   E           K DISK
002900911017     FTABEL00F  IF   E           K DISK
003100050412     Ftigcp00f  uF   E             DISK
005300040115     D DS2z          E DS
005400040116     D DS3a          E DS
005901050412     D WLBDA8          DS
005902050412     D  G02DAT                 1      8  0
005903050412     D  G02INV                 9     16  0
005904050412     D  G02ERR                17     17
005905050412     D  G02TGI                18     22  0
005906050412     d aas             s                   like(gcpaasa)
005907050412     d lnp             s                   like(gcplnpa)
005908050412     d nrs             s                   like(gcpnrsa)
005909050412     d nsp             s                   like(gcpnspa)
005910050412     d cmc             s                   like(gcpcmc)
005911050412     d rrn             s              9  0
005912050412     c*
005913050412     C/EXEC SQL
005914050412     C+ DECLARE C1 CURSOR FOR SELECT GCPAASa, GCPLNPa, GCPNRSa, GCPNSPa,
005915050412     C+ gcpcmc, rrn(tigcp00f) FROM tiGCP00f WHERE GCPFAS=20 AND
005916050412     C+ GCPDUR>=20050411 AND GCPFRG=0 AND GCPDED>0
005917050412     C/END-EXEC
005918050412     c*
005919050412     C/EXEC SQL
005920050412     C+ OPEN C1
005921050412     C/END-EXEC
005922050412
005923050412     C                   DO        *HIVAL
005924050412
005925050412     C/EXEC SQL
005926050412     C+ FETCH NEXT FROM C1 INTO :aas, :lnp, :nrs, :nsp, :cmc, :rrn
005927050412     C/END-EXEC
005928050412
005929050412     C                   IF        SQLCod = 100
005930050412     C                   LEAVE
005931050412     C                   ENDIF
005932050412     c     karb          chain     fnarb01l
005933050412     C                   IF        %FOUND(FNARB01L)
005936050412     c* controllo se è una data consegna richiesta
005937050412     C                   MOVE      '2Z'          COD
005938050412     C                   MOVEL(p)  cmc           KEY
005939050412     C     KTAB          CHAIN     TABEL00F                           33
005940050412     C     *IN33         IFEQ      '0'
005941050412     C                   MOVEL     TBLUNI        DS2z
005942050412     c                   if        §2zdcr <> *blank
005943050412     C     rrn           chain     tigcp00f
005944050412     c                   if        %found(tigcp00f)
005945050412     c*
030200040115     c                   exsr      datarich
030600040827      * verifico se cliente vario
030700040827     c                   move      gcpscm        cod4              4 0
031200040827      * se cliente vario non imposto la data disposizioni in modo che resti
031300050412      * in evidenza alla filiale
031500040924     c                   if        cod4  = 8888 or cod4 = 9999
031600050412     C                   z-add     0             GCPDDM
031601050412     C                   z-add     0             GCPDLD
031602050412     C                   z-add     25            GCPfas
031700040827     c                   else
031701050412     C                   z-add     30            GCPfas
031702050412     C                   z-add     dateu         GCPDLD
031703050412     c                   if        gcpddm = 0
031800050412     C                   z-add     dateu         GCPDDM
031900040827     c                   endif
031901050412     c                   endif
032000040827
032400040115     C                   MOVE      'C'           GCPDIS
036000031027      *
036102050412     C                   update    tigcp000
036103050412     c                   end
036104050412     c                   end
036105050412     c                   end
036106050412     c                   end
036107050412     C                   ENDDO
036108050412
036109050412     C/EXEC SQL
036110050412     C+ CLOSE C1
036111050412     C/END-EXEC
049500040115
049600000000     C                   SETON                                        LR
049700040115
049800040115     C**---------------------------------------------------------
049900040115     c     datarich      begsr
050000040115     C**---------------------------------------------------------
050001050412     c*
051000040116      * controllo il tipo bolla per l'attribuzione delle spese e provvigioni
051100040116     C                   MOVE      '3A'          COD
051300050412     C                   MOVEL(p)  arbcbo        KEY
051400050412     C     KTAB          CHAIN     TABEL00F
051500050412     C                   IF        %found(tabel00f)
051600040116     C                   MOVEL     TBLUNI        DS3a
051700040116     C                   MOVEL     §3ATB1        PORT              1
051701050412     c                   move      'S'           gcpasg
051800040116      * spese trasporto
051900040116     c                   if        port = 'F'
052000040116     c                   move      'M'           gcppsg
052200040116     c                   else
052300040116     c                   move      'D'           gcppsg
052500040116     c                   endif
052600040116      * provv. contrassegno
052601050412     c                   clear                   gcpppc
052602050412     c                   clear                   gcpapc
052603050412     c                   if        §3afca <> 0
052604050412     c                   move      'S'           gcpapc
052605050412     c                   if        port = 'F'
052606050412     c                   if        §3atb2 <> *blank
052607050412     c                   move      'D'           gcpppc
052608050412     c                   else
052609050412     c                   move      'M'           gcpppc
052611050412     c                   endif
052612050412     c                   else
052613050412     c                   move      'D'           gcpppc
052615050412     c                   endif
052616050412     c                   endif
054600040116
054900040115     C                   END
055000040115     c                   endsr
075200941013     C*------------------------------------------------------------*
075300941013     C*  OPERAZIONI INIZIALI                          -------------*
075400941013     C*------------------------------------------------------------*
075500941013     C     *INZSR        BEGSR
075900941013     C* COMPONGO LA DATA ROVESCIATA
076000941013     C                   TIME                    WHHDAT           14 0
076100941013     C                   MOVE      WHHDAT        G02DAT
076200941013     C                   MOVE      *ZEROS        G02INV
076300941013     C                   MOVEL     ' '           G02ERR
076400941013     C                   CALL      'XSRDA8'
076500941013     C                   PARM                    WLBDA8
076600941013     C                   Z-ADD     G02INV        DATEU             8 0
077400041001     C*
077500941013     C* DEFINIZIONE CHIAVI
077600941013     C*
077700050412     C     Karb          KLIST
077800050412     C                   KFLD                    AAS
077900050412     C                   KFLD                    LNP
078000050412     C                   KFLD                    NRS
078100050412     C                   KFLD                    NSP
080900031030     C*
081000941013     C     KTAB          KLIST
081100941013     C                   KFLD                    UT                1 0
081200941013     C                   KFLD                    COD               2
081300941013     C                   KFLD                    KEY               8
081400941013     C                   MOVEL     '1'           UT
085300941013     C                   ENDSR
