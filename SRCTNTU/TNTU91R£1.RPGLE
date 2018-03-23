000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200060105     H* TNTU91R *----------------------------------------------------*
000300060104     H* - LEGGE bolle arrivo per data arrivo     - fnarb
000400000000     H*--------------------------------------------------------------*
000500051214     ftntud01l  uf a e           k disk
000600060104     ffnarb29l  if   e           k Disk
000700060220     ffIar401l  if   e           k Disk
000800060104     ffiar501l  if   e           k Disk
000900060110     ffiar601l  if   e           k Disk
001000060104     ffiar901l  if   e           k Disk
001100060109     ftabel00f  if   e           k disk
001200051215     d
001300060110     D CBO             S              2    DIM(50)
001400060110     D ASS             S              2    DIM(50)
001500060110     D ASS2            S              2    DIM(50)
001600060111     D REC             S              2    DIM(50)
001700060109     D TFA             S              3  0 DIM(50)
001800060109     D TFAVAL          S              9  0 DIM(50)
001900060109     d
002000051214     dkpjba          e ds
002100051214     dtntu05ds       e ds
002200060103     dtntudtads      e ds
002300060105     ddsbl4a         e ds
002400060105     ddar5gen        e ds
002500060109     dtrul21ds       e ds
002600060109     dds3a           e ds
002700060320     D* DS PER FNLV80R - per verificare se tassativa
002800060320     D FNLV80DS      E DS
002900051215     d
003000060105     d Kdata           s                   Like(arbdam)
003100060109     d Kfgs            s                   Like(arblna)
003200060109     d savfgs          s                   Like(arblna)
003300051215     d Kmult           s                   Like(tudmult)
003400060105     d contaddtno      s                   Like(tudqta)
003500060105     d contaddtsi      s                   Like(tudqta)
003600060105     d contaddtsino    s                   Like(tudqta)
003700060105     d contavalore     s                   Like(tudqta)
003800060105     d contaCA         s                   Like(tudqta)
003900060105     d contaAppunt     s                   Like(tudqta)
004000060105     d contaSuper      s                   Like(tudqta)
004100060105     d contaffd        s                   Like(tudqta)
004200060110     d contaSF         s                   Like(tudqta)
004300060110     d contaFImm       s                   Like(tudqta)
004400060323     d contaColli      s                   Like(tudqta)
004500060323     d conta           s                   Like(tudqta)
004600060110     d ktrc            s                   Like(ar4trc)
004700060105     D ktrd            s                   like(ar5trd) inz('GEN')
004800051215     d Kcli            s                   Like(tudcli) inz(0)
004900060109     d ktbkut          s                   like(tblkut) inz(1)
005000060109     d ktbcod          s                   like(tblcod) inz('3A')
005100051215     d Dataiso         s               d   datfmt(*iso)
005200060109     d Datafine        s               d   datfmt(*iso)
005300051220     d
005400900518     C***************************************************************
005500051214     c* Moltiplicatori calcolati:
005600060105     c*    030 - DDT SI + DDT NO
005700060105     c*    050 - DDT NO
005800060105     c*    052 - DDT SI
005900060109     c*    080 - assegnati segue fattura
006000060109     c*    082 - assegnati fattura immediata
006100060104     c*    160 - colli di valore
006200060323     c*    165 - colli arrivati
006300060105     c*    540 - telefonate per C/assegno
006400060105     c*    550 - telefonate per appuntamento
006500060105     c*    560 - telefonate per supermercati
006600060105     c*    570 - telefonate per Fermo deposito
006700060105     C***************************************************************
006800060110     c* Carico i cod bolla con C/Assegno  e in assegnato
006900060109     c                   exsr      carCBO
007000060109     c
007100060109    1c                   if        t05mult='160' or t05mult=*blanks
007200060109     c                   eval      kmult='160'
007300060109     c* loop sulla data
007400060109     c     *iso          movel     t05dti        dataiso
007500060109     c     *iso          movel     t05dtf        datafine
007600060109    2c                   dow       dataiso<=datafine
007700060109     c                   exsr      CANCMULT
007800060109     c
007900060109     c                   adddur    1:*D          dataiso
008000060109    2c                   enddo
008100060109    1c                   endif
008200060109     c
008300060105     c* Cerco i p.o. perchè la lf è per linea /data
008400060109    1c                   if        t05fil=0
008500051220     c
008600060105     c     kfgs          setgt     fnarb29l
008700060105     c                   read      fnarb29l
008800051220     c
008900051219     c                   else
009000060105     c     kfgs          setll     fnarb29l
009100060105     c     kfgs          reade     fnarb29l
009200060109    1c                   endif
009300051219     c
009400060109    1c                   dow       not %eof(fnarb29l)
009500060105     c                   z-add     arblna        kfgs
009600060105     c                   clear                   contaddtno
009700060105     c                   clear                   contaddtsi
009800060105     c                   clear                   contaddtsino
009900060105     c                   clear                   contavalore
010000060105     c                   clear                   contaCA
010100060105     c                   clear                   contaAppunt
010200060105     c                   clear                   contaSuper
010300060105     c                   clear                   contaFFD
010400060109     c                   clear                   tfa
010500060109     c                   clear                   tfaval
010600060110     c                   clear                   contaSF
010700060110     c                   clear                   contafimm
010800060323     c                   clear                   contacolli
010900051215     c
011000051215     c* Imposta il giro sulla data
011100051215     c                   z-add     t05dti        kdata
011200060109    2c                   dow       kdata<=t05dtf
011300060105     c     karb          setll     fnarb29l
011400060105     c     karb          reade     fnarb29l
011500051220
011600060109    3c                   dow       not %eof(fnarb29l)
011700060111     c
011800060111     c* Spedizioni in assegnato
011900060111     c     arbcbo        lookup    ass                                    31
012000060111     c     arbcbo        lookup    ass2                                   32
012100060111     c                   if        *in31
012200060111     c                   eval      ktrc='1'
012300060111     c                   endif
012400060111     c                   if        *in32
012500060111     c                   eval      ktrc='2'
012600060111     c                   endif
012700060111     c
012800060111     c                   if        *in31 or *in32
012900060111     c     kar4          chain     fiar601l
013000060111     c                   if        %found(fiar601l)  and ar6dft>0
013100060111     c                   add       1             contaFIMM
013200060111     c                   else
013300060111     c                   add       1             contaSF
013400060111     c                   endif
013500060111     c                   endif
013600060111     c
013700060111     c* Per tutti gli altri conteggi, escludo i recuperi
013800060111     c     arbcbo        lookup    rec                                    31
013900060111     c                   if        not *in31
014000060105     c
014100060220     c* conto i ddt si/ddt no chainando fIar4
014200060109     c                   clear                   dsbl4a
014300060110     c                   eval      ktrc='A'
014400060220     c     kar4          chain     fIar401l
014500060220    4c                   if        %found(fIar401l)
014600060105     c                   movel     ar4not        dsbl4a
014700060109    4c                   endif
014800060105     c
014900060105     c* ddi si x stampa ldv
015000060105     c                   if        §b4abm='P' or §b4abm='Q'
015100060105     c                   add       1             contaddtsi
015200060105     c                   endif
015300060105     c* ddi no x stampa ldv
015400060105     c                   if        §b4abm='J' or §b4abm='W'
015500060105     c                   add       1             contaddtno
015600060105     c                   endif
015700060105     c
015800060105     c* ddt si + ddt no x smistamento ldv
015900060105     c                   add       1             contaddtsino
016000060323     c* Colli arrivati
016100060323     c                   add       arbncl        contacolli
016200060105     c* Vedo se colli di valore
016300060105     c                   clear                   dar5gen
016400060105     c     kar5          chain     fiar501l
016500060105     c                   if        %found(fiar501l)
016600060105     c                   movel     ar5uni        dar5gen
016700060105     c                   endif
016800060105     c
016900060109    4c                   if        §ar5bva='V'
017000060109     c                   add       arbncl        contavalore
017100060109     c* Memorizzo anche per terminal di arrivo
017200060109    5c                   if        arbtfa<>arblna
017300060109     c                   z-add     1             yy
017400060109     c     arbtfa        lookup    tfa(YY)                                31
017500060109    6c                   if        not *in31
017600060109     c                   z-add     1             yy
017700060109     c     000           lookup    tfa(YY)                                31
017800060109     c                   eval      tfa(yy)=arbtfa
017900060109    6c                   endif
018000060109     c                   add       arbncl        tfaval(yy)
018100060109    5c                   endif
018200060109     c
018300060109    4c                   endif
018400060109     c
018500060109     c     arbcbo        lookup    cbo                                    31
018600060109    4c                   if        *in31
018700060109     c     kar9          chain     fiar901l
018800060109    5c                   if        %found(fiar901l)
018900060109      * NUOVO CONTROLLO C/ASSEGNO
019000060109     c                   clear                   trul21ds
019100060109     c                   eval      i21tla = 'L'
019200060109     c                   eval      i21cbo = arbcbo
019300060109     c                   eval      i21tsp = arbtsp
019400060109     c                   eval      i21lnp = arblnp
019500060109     c                   eval      i21nzm = arbnzm
019600060109     c                   eval      i21lna = arblna
019700060109     c                   eval      i21nzd = arbnzd
019800060109     c                   eval      i21tic = ar9tic
019900060109     c                   eval      i21imp = ar9cas
020000060109     c                   eval      i21div = ar9vca
020100060109     c                   eval      i21ute = knmus
020200060109     c                   eval      i21pgm = 'TNTU91R'
020300060109     c                   call      'TRUL21R'
020400060109     c                   parm                    trul21ds
020500060109
020600060109     c                   if        o21fca<> *blanks
020700060109     c                   add       1             contaCA
020800060109     c                   endif
020900060109     c
021000060109    5c                   endif
021100060109    4c                   endif
021200060109     c
021300060109     c                   if        arbtc1='A' or arbtc2='A'
021400060109     c                   add       1             contaAppunt
021500060109     c                   endif
021600060105     c
021700060109     c                   if        arbtc1='S' or arbtc2='S'
021800060320     c* EScludo le tassative
021900060320     c                   if        arbdcr>0 or arbhcr>0
022000060320     c                   clear                   fnlv80ds
022100060320     c                   eval      ilv80aas=arbaas
022200060320     c                   eval      ilv80lnp=arblnp
022300060320     c                   eval      ilv80nrs=arbnrs
022400060320     c                   eval      ilv80nsp=arbnsp
022500060320     c                   eval      ilv80mgs=arbmgs
022600060320     c                   eval      ilv80tc1=arbtc1
022700060320     c                   eval      ilv80tc2=arbtc2
022800060320     c                   eval      ilv80dcr=arbdcr
022900060320     c                   eval      ilv80hcr=arbhcr
023000060320     c                   eval      ilv80tcr=arbtcr
023100060320     c                   call      'FNLV80R'
023200060320     c                   parm                    kpjba
023300060320     c                   parm                    fnlv80ds
023400060320     c
023500060320     c                   if        olv80tat<>'S'
023600060109     c                   add       1             contaSuper
023700060320     c                   endif
023800060320     c                   else
023900060320     c                   add       1             contaSuper
024000060320     c                   endif
024100060320     c
024200060109     c                   endif
024300060109     c
024400060109     c                   if        arbffd='S'
024500060109     c                   add       1             contaFFD
024600060109     c                   endif
024700060111     c                   endif
024800060105     c
024900051219
025000060105     c     karb          reade     fnarb29l
025100060109    3c                   enddo
025200051219     c
025300060120     c                   if        t05mult=*blanks or  t05mult='570'
025400060109     c                   z-add     contaffd      conta
025500060109     c                   eval      kmult='570'
025600051215     c                   exsr      SCriviTUD
025700060120     c                   endif
025800060109     c
025900060120     c                   if        t05mult=*blanks or  t05mult='560'
026000060109     c                   z-add     contaSuper    conta
026100060109     c                   eval      kmult='560'
026200060109     c                   exsr      SCriviTUD
026300060120     c                   endif
026400060109     c
026500060120     c                   if        t05mult=*blanks or  t05mult='550'
026600060109     c                   z-add     contaAppunt   conta
026700060109     c                   eval      kmult='550'
026800060109     c                   exsr      SCriviTUD
026900060120     c                   endif
027000060109     c
027100060120     c                   if        t05mult=*blanks or  t05mult='540'
027200060109     c                   z-add     contaCa       conta
027300060109     c                   eval      kmult='540'
027400060109     c                   exsr      SCriviTUD
027500060120     c                   endif
027600060109     c
027700060120     c                   if        t05mult=*blanks or  t05mult='052'
027800060109     c                   z-add     contaDDTSI    conta
027900060109     c                   eval      kmult='052'
028000060109     c                   exsr      SCriviTUD
028100060120     c                   endif
028200060109     c
028300060120     c                   if        t05mult=*blanks or  t05mult='050'
028400060109     c                   z-add     contaDDTno    conta
028500060109     c                   eval      kmult='050'
028600060109     c                   exsr      SCriviTUD
028700060120     c                   endif
028800060109     c
028900060120     c                   if        t05mult=*blanks or  t05mult='030'
029000060109     c                   z-add     contaDDTSIno  conta
029100060109     c                   eval      kmult='030'
029200060109     c                   exsr      SCriviTUD
029300060120     c                   endif
029400060110     c
029500060120     c                   if        t05mult=*blanks or  t05mult='080'
029600060110     c                   z-add     contaSF       conta
029700060110     c                   eval      kmult='080'
029800060110     c                   exsr      SCriviTUD
029900060120     c                   endif
030000060109     c
030100060120     c                   if        t05mult=*blanks or  t05mult='082'
030200060110     c                   z-add     contaFIMM     conta
030300060110     c                   eval      kmult='082'
030400060109     c                   exsr      SCriviTUD
030500060120     c                   endif
030600060323     c                   if        t05mult=*blanks or  t05mult='165'
030700060323     c                   z-add     contaCOLLI    conta
030800060323     c                   eval      kmult='165'
030900060323     c                   exsr      SCriviTUD
031000060323     c                   endif
031100060110
031200060120     c                   if        t05mult=*blanks or  t05mult='160'
031300060110     c                   z-add     contaValore   conta
031400060110     c                   eval      kmult='160'
031500060110     c                   exsr      SCriviTUD
031600060109     c* Sommo anche i colli per il terminal di arrivo
031700060109     c                   eval      savfgs=kfgs
031800060109     c
031900060109     c                   z-add     1             yy
032000060109     c                   dow       tfa(YY)>0
032100060109     c                   z-add     tfa(yy)       kfgs
032200060109     c                   z-add     tfaval(yy)    conta
032300060109     c                   exsr      SCriviTUD
032400060109     c                   add       1             yy
032500060109     c                   enddo
032600060109     c
032700060109     c                   eval      kfgs=savfgs
032800060120     c                   endif
032900060105     c
033000060105     c                   clear                   contaddtno
033100060105     c                   clear                   contaddtsi
033200060105     c                   clear                   contaddtsino
033300060105     c                   clear                   contavalore
033400060105     c                   clear                   contaCA
033500060105     c                   clear                   contaAppunt
033600060105     c                   clear                   contaSuper
033700060105     c                   clear                   contaFFD
033800060109     c                   clear                   tfa
033900060109     c                   clear                   tfaval
034000060110     c                   clear                   contaSF
034100060110     c                   clear                   contaFIMM
034200060323     c                   clear                   contaColli
034300051215     c
034400051219     c
034500051219     c* Aggiungo 1 giorno alla data
034600051219     c     *iso          movel     kdata         dataiso
034700051219     c                   adddur    1:*D          dataiso
034800051219     c                   movel     dataiso       kdata
034900060109    2c                   enddo
035000051215     c
035100051219     c                   if        t05fil=0
035200051220     c
035300060105     c     kfgs          setgt     fnarb29l
035400060105     c                   read      fnarb29l
035500051219     c                   else
035600051219     c* Se impostato un p.o. finito la lettura
035700051219     c                   leave
035800051219     c                   endif
035900051219     c
036000060109    1c                   enddo
036100051215     c
036200060111     c* richiamo pgm tntu91r per finire di scrivere i segue fattura
036300060111     c*  recuperi in assegnato
036400060111     c                   if        t05mult=*blanks or t05mult='080'
036500060111     c                   movea     rec           codbollarec     100
036600060111     c                   call      'TNTU90R'
036700060111     c                   parm                    kpjba
036800060111     c                   parm                    codbollarec
036900060111     c                   endif
037000060111     c
037100060103     c
037200060103     c* Se non richiesto un moltiplicatore o un p.o., aggiorno la tabella
037300060103     c*  TUP
037400060103     c                   if        t05mult=*blanks and t05fil=0
037500060103     c                   clear                   tntudtads
037600060103     c                   eval      §DTAazn = kcoaz
037700060103     c                   eval      §DTAdta = t05dtf
037800060103     c                   call      'TNTUDTAR'
037900060103     c                   parm                    tntudtads
038000060103     c                   endif
038100941216     C*
038200051123     C                   SETON                                        LR
038300991206     C**************************************************************************
038400051123     C     *INZSR        BEGSR
038500051214     c
038600051214     C     *ENTRY        PLIST
038700051214     C                   PARM                    kpjba
038800051214     c                   movel     kpjbu         tntu05ds
038900051123     c
039000060109     c     ktud2         Klist
039100060109     c                   Kfld                    kmult
039200060109     c                   Kfld                    dataiso
039300060109     C     KE2TAB        KLIST
039400060109     C                   KFLD                    KTBKUT
039500060109     C                   KFLD                    KTBCOD
039600060109     C     KAR9          KLIST
039700060109     C                   KFLD                    arbaas
039800060109     C                   KFLD                    arbLNP
039900060109     C                   KFLD                    arbNRS
040000060109     C                   KFLD                    arbNSP
040100060105     C     KAR4          KLIST
040200060105     C                   KFLD                    arbaas
040300060105     C                   KFLD                    arbLNP
040400060105     C                   KFLD                    arbNRS
040500060105     C                   KFLD                    arbNSP
040600060105     C                   KFLD                    KTRC
040700060105     C     KAR5          KLIST
040800060109     C                   KFLD                    arbAAS
040900060109     C                   KFLD                    arbLNP
041000060109     C                   KFLD                    arbNRS
041100060109     C                   KFLD                    arbNSP
041200060105     C                   KFLD                    ktrd
041300060105     c     karb          Klist
041400051219     c                   Kfld                    kfgs
041500051214     c                   Kfld                    kdata
041600051215     c     ktud          Klist
041700051215     c                   Kfld                    kmult
041800051215     c                   Kfld                    dataiso
041900051219     c                   Kfld                    kfgs
042000051215     c                   Kfld                    kcli
042100051214     c
042200060109     c* Considero il p.o. solo se richiesto un moltiplicatore <> da '160'
042300060109     c*  e diverso da blanks cioè tutti
042400060109     c                   if        t05fil>0  and t05mult<>*blanks  and
042500060109     c                             t05mult<>'160'
042600051219     c                   eval      kfgs=t05fil
042700060109     c                   else
042800060109     c                   clear                   t05fil
042900060109     c                   clear                   kfgs
043000051219     c                   endif
043100051219     c
043200051124    cc                   endsr
043300051215     C**************************************************************************
043400051215     c* Scrivo il file TNTUD00F
043500051215     C**************************************************************************
043600051215     C     SCriviTUD     BEGSR
043700051215     c     *iso          movel     kdata         dataiso
043800051215     c
043900060109     c                   if        conta>0
044000051215     c
044100051215     c     ktud          chain     tntud01l
044200051215     c
044300060109     c                   if        %found(tntud01l)  and kmult='160'
044400060111     c                   add       conta         tudqta
044500060109     c                   else
044600060111     c                   z-add     conta         tudqta
044700060109     c                   endif
044800051215     c
044900051215     c                   if        not %found(tntud01l)
045000051215     c                   eval      tuddta=dataiso
045100051215     c                   eval      tudmult=kmult
045200051219     c                   eval      tudfil=kfgs
045300051215     c                   clear                   tudcli
045400051215     c                   write     tntud000
045500051215     c                   else
045600051215     c                   update    tntud000
045700051215     c                   endif
045800051215     c                   endif
045900051215     c
046000051215     c                   ENDSR
046100060109      *-----------------------------------------------------------------*
046200060109     c* Carico cod bolla con C/A e in assegnato
046300060109      *-----------------------------------------------------------------*
046400060109     c     CarCBO        begsr
046500060109     C                   CLEAR                   II                4 0
046600060109     C                   CLEAR                   yy                4 0
046700060110     C                   CLEAR                   zz                4 0
046800060111     C                   CLEAR                   xx                4 0
046900060109     C     KE2TAB        SETLL     TABEL00F
047000060109     C     KE2TAB        READE     TABEL00F                               98
047100060109     C     *IN98         DOWEQ     *OFF
047200060109     C                   MOVEL     TBLUNI        DS3a
047300060109     C     §3afca        IFne      0
047400060109     C                   ADD       1             II
047500060109     C                   MOVEL     TBLKEY        CBO(II)
047600060109     C                   ENDIF
047700060109     c
047800060109     c                   movel     §3atb1        flgtb1            1
047900060110     c                   if        flgtb1='A'
048000060109     C                   ADD       1             yy
048100060109     C                   MOVEL     TBLKEY        ass(yy)
048200060109     c                   endif
048300060110     c                   if        §3atb2<>*blanks
048400060110     C                   ADD       1             zz
048500060110     C                   MOVEL     TBLKEY        ass2(zz)
048600060110     c                   endif
048700060111     c
048800060111     c                   if        §3arbl='R'
048900060111     C                   ADD       1             xx
049000060111     C                   MOVEL     TBLKEY        rec(xx)
049100060111     c                   endif
049200060111     c
049300060109     C     KE2TAB        READE     TABEL00F                               98
049400060109     C                   ENDDO
049500060109     C*
049600060109     C                   ENDSR
049700060109     C**************************************************************************
049800060109     c* Cancella il moltiplicatore se rielaborazione
049900060109     C**************************************************************************
050000060109     C     CANCMULT      BEGSR
050100060109     c     ktud2         setll     tntud01l
050200060109     c
050300060109     c                   setoff                                       31
050400060109     c                   dow       not *in31
050500060109     c     ktud2         delete    tntud01l                           31
050600060109     c                   enddo
050700060109     c
050800060109     c                   ENDSR
