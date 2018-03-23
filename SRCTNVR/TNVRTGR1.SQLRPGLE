000100921203     H DECEDIT('0,') DATEDIT(*DMY.)
003100050420     Ftigcp00f  uF   E             DISK
003101050413     fprtf198   o    f  198        printer
005300040115     D DS2z          E DS
005400040116     D DS3a          E DS
005401050413     d arrn            s              9  0
005402050413     d aagc            s                   like(gcpagc)
005403050413     d afgc            s                   like(gcpfgc)
005404050413     d angc            s                   like(gcpngc)
005405050413     d asgs            s                   like(gcpsgs)
005406050413     d asgr            s                   like(gcpsgr)
005407050413     d asgd            s                   like(gcpsgd)
005408050413     d adsd            s                   like(gcpdsd)
005409050413     d adsa            s                   like(gcpdsa)
005410050413     d aggs            s                   like(gcpggs)
005411050413     d asgf            s                   like(gcpsgf)
005412050413     d asga            s                   like(gcpsga)
005413050413     d atft            s                   like(gcptft)
005414050413     d aksc            s                   like(gcpksc)
005415050413     d adft            s                   like(gcpdft)
005416050413     d anft            s                   like(gcpnft)
005417050413     d afiv            s                   like(gcpfiv)
005418050413     d bsgs            s                   like(gcpsgs)
005419050413     d bsgr            s                   like(gcpsgr)
005420050413     d bsgd            s                   like(gcpsgd)
005421050413     d bdsd            s                   like(gcpdsd)
005422050413     d bdsa            s                   like(gcpdsa)
005423050413     d bggs            s                   like(gcpggs)
005424050413     d bsgf            s                   like(gcpsgf)
005425050413     d bsga            s                   like(gcpsga)
005426050413     d btft            s                   like(gcptft)
005427050413     d basw            s                   like(gcpasw)
005428050413     d blpw            s                   like(gcplpw)
005429050413     d bnsw            s                   like(gcpnsw)
005430050413     d bnbw            s                   like(gcpnbw)
005431050413     d bksc            s                   like(gcpksc)
005432050413     d bdft            s                   like(gcpdft)
005433050413     d bnft            s                   like(gcpnft)
005434050413     d bfiv            s                   like(gcpfiv)
005912050412     c*
005913050412     C/EXEC SQL
005914050413     C+ DECLARE C1 CURSOR FOR SELECT A.GCPAGC, A.GCPFGC, A.GCPNGC,
005915050413     C+ A.GCPSGS, A.GCPSGR, A.GCPSGD, A.GCPDSD, A.GCPDSA, A.GCPGGS,
005916050413     C+ A.GCPSGF, A.GCPSGA, A.GCPTFT, A.GCPKSC, A.GCPDFT, A.GCPNFT,
005917050413     C+ A.GCPFIV, B.GCPSGS, B.GCPSGR, B.GCPSGD, B.GCPDSD, B.GCPDSA,
005918050413     C+ B.GCPGGS, B.GCPSGF, B.GCPSGA, B.GCPTFT, B.GCPLPW, B.GCPASW,
005919050413     C+ B.GCPNSW, B.GCPNBW, B.GCPKSC, B.GCPDFT, B.GCPNFT, B.GCPFIV,
005920050413     C+ rrn(a) FROM tigcp00f a join inv050410/figcp00f b on
005921050413     C+ a.gcpagc=b.gcpagc and a.gcpfgc=b.gcpfgc and a.gcpngc=b.gcpngc
005922050420     C+ WHERE a.gcpfas<= 40 and a.gcpnbw <> 0 and a.gcpnbw <> 9999999 and
005923050413     C+ b.gcpnbw = a.gcpnbw and b.gcpasw = a.gcpasw and b.gcplpw =
005924050413     C+ a.gcplpw and b.gcpnsw = a.gcpnsw and (
005925050420     C*                              A.GCPdsd<> b.GCPdsd OR
005926050420     C*                              A.GCPdsa<> b.GCPdsa OR
005927050420     C*                              A.GCPggs<> b.GCPggs OR
005928050420     C*                              A.GCPsgf<> b.GCPsgf OR
005929050420     C*                              A.GCPtft<> b.GCPtft OR
005930050420     C*                              A.GCPksc<> b.GCPksc OR
005931050420     C+                              A.GCPksc<> b.GCPksc
005932050420     C*                              A.GCPdft<> b.GCPdft OR
005933050420     C*                              A.GCPnft<> b.GCPnft OR
005934050420     C*                              A.GCPfiv<> b.GCPfiv )
005935050420     C+                                                  )
005936050413     C/END-EXEC
005938050412     c*
005939050412     C/EXEC SQL
005940050412     C+ OPEN C1
005941050412     C/END-EXEC
005942050412
005943050412     C                   DO        *HIVAL
005944050412
005945050412     C/EXEC SQL
005946050413     C+ FETCH NEXT FROM C1 INTO :aagc       ,
005947050413     C+                         :afgc       ,
005948050413     C+                         :angc       ,
005949050413     C+                         :asgs       ,
005950050413     C+                         :asgr       ,
005951050413     C+                         :asgd       ,
005952050413     C+                         :adsd       ,
005953050413     C+                         :adsa       ,
005954050413     C+                         :aggs       ,
005955050413     C+                         :asgf       ,
005956050413     C+                         :asga       ,
005957050413     C+                         :atft       ,
005958050413     C+                         :aksc       ,
005959050413     C+                         :adft       ,
005960050413     C+                         :anft       ,
005961050413     C+                         :afiv       ,
005962050413     C+                         :bsgs       ,
005963050413     C+                         :bsgr       ,
005964050413     C+                         :bsgd       ,
005965050413     C+                         :bdsd       ,
005966050413     C+                         :bdsa       ,
005967050413     C+                         :bggs       ,
005968050413     C+                         :bsgf       ,
005969050413     C+                         :bsga       ,
005970050413     C+                         :btft       ,
005972050413     C+                         :blpw       ,
005973050413     C+                         :basw       ,
005974050413     C+                         :bnsw       ,
005975050413     C+                         :bnbw       ,
005976050413     C+                         :bksc       ,
005977050413     C+                         :bdft       ,
005978050413     C+                         :bnft       ,
005979050413     C+                         :bfiv       ,
005980050413     C+                         :arrn
006153050413     C/END-EXEC
006155050412     C                   IF        SQLCod = 100
006156050412     C                   LEAVE
006157050412     C                   ENDIF
006158050413     C     arrn          chain     tigcp00f
006159050413     c                   if        %found(tigcp00f)
006160050413     c                   if        gcpdsd = 0
006161050413     c                   eval      gcpdsd = bdsd
006162050413     c                   end
006163050413     c                   if        gcpdsa = 0
006164050413     c                   eval      gcpdsa = bdsa
006165050413     c                   end
006166050413     c                   if        gcpggs = 0
006167050413     c                   eval      gcpggs = bggs
006168050413     c                   end
006169050413     c                   if        gcpsgf = 0
006170050413     c                   eval      gcpsgf = bsgf
006171050413     c                   end
006172050413     c                   if        gcptft = 0
006173050413     c                   eval      gcptft = btft
006174050413     c                   end
006175050412     c                   if        gcpksc = 0
006176050413     c                   eval      gcpksc = bksc
006177050412     c                   end
006178050412     c                   if        gcpdft = 0
006179050413     c                   eval      gcpdft = bdft
006180050412     c                   end
006181050412     c                   if        gcpnft = 0
006182050413     c                   eval      gcpnft = bnft
006183050412     c                   end
006184050412     c                   if        gcpfiv = 0
006185050413     c                   eval      gcpfiv = bfiv
006186050412     c                   end
036102050420     C                   update    tigcp000
036103050413     C                   except    stampa
036104050412     c                   end
036107050412     C                   ENDDO
036108050412
036109050412     C/EXEC SQL
036110050412     C+ CLOSE C1
036111050412     C/END-EXEC
049500040115
049600000000     C                   SETON                                        LR
049700040115
075200941013     C*------------------------------------------------------------*
075300941013     C*  OPERAZIONI INIZIALI                          -------------*
075400941013     C*------------------------------------------------------------*
075500941013     C     *INZSR        BEGSR
075501050413     c                   endsr
077400041001     C*
077500050413     oprtf198   e            stampa      1
077501050413     o                                           +0 'Giac.TIGCP'
077600050413     o                       aagc                +1
077700050413     o                       afgc                +1
077800050413     o                       angc          z     +1
077801050413     o                                           +1 'Spese'
077900050413     o                       asgs          4     +1
078000050413     o                       asgr          4     +1
078100050413     o                       asgd          4     +1
078101050413     o                       asga          4     +1
078102050413     o                                           +1 'Dt e gg sosta'
078200050413     o                       adsd                +1
078300050413     o                       adsa                +1
078400050413     o                       aggs          4     +1
078500050413     o                       asgf                +1
078501050413     o                                           +1 'Tp fat.'
078700050413     o                       atft                +1
078701050413     o                                           +1 'Cli.fat.'
078800050413     o                       aksc                +1
078801050413     o                                           +1 'Fattura'
078900050413     o                       adft                +1
079000050413     o                       anft          z     +1
079100050413     o                       afiv                +1
079101050413     o                                           +1 'RRN'
079102050413     o                       aRRN                +1
079103050413     o          e            stampa      1
079104050413     o                                           +0 'Bolla'
079105050413     o                       basw                +1
079106050413     o                       blpw                +1
079107050413     o                       bnsw          z     +1
079108050413     o                       bnbw          z     +1
079200050413     o                       bsgs          4     +9
079300050413     o                       bsgr          4     +1
079400050413     o                       bsgd          4     +1
079401050413     o                       bsga          4     +1
079500050413     o                       bdsd               +15
079600050413     o                       bdsa                +1
079700050413     o                       bggs          4     +1
079800050413     o                       bsgf                +1
080000050413     o                       btft                +9
080100050413     o                       bksc               +10
080200050413     o                       bdft                +9
080300050413     o                       bnft          z     +1
080400050413     o                       bfiv                +1
