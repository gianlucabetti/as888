000100040119      *===============================================================*
000200130125      * TICF93R    * Confronto fatturazione (3) + varie (progetto 810)
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000130125     fwfcft11l  if   e           k disk
001100130125     fwfcft30f  uf a e             disk
001200120208
001300050609
001400050614     d param           ds
001500050614     D  pardrf                 1      8  0
001600050614     D  paraam                 9     14  0
001700130128     D  paranno                9     12  0
001800130128     D  parmese               13     14  0
001900130128     d  pilota                35     35
002000120208
002100050614     d kpjba         e ds
002200120208
002300120208
002400130128     D wtos            s                   like(cft3tos)
002500130128     D wnspsaldi       s                   like(cft3nsp)
002600130128     D wtop            s                   like(cft3top)
002700130128     D wtoc            s                   like(cft3toc)
002800130125     D wistatp         s                   like(cft3istatp)
002900130125     D wfuelp          s                   like(cft3fuelp)
003000130128     D wistatc         s                   like(cft3istatc)
003100130128     D wfuelc          s                   like(cft3fuelc)
003200130128     D wimpistat       s                   like(cft3impist)
003300130128     D wimpfuel        s                   like(cft3impfue)
003400120208
003500130128     D s_cft1kun       s                   like(cft1kun)
003600130128     D s_cft1kund      s                   like(cft1kund)
003700130128     D s_cft1dit       s                   like(cft1dit)
003800130128     D s_cft1ditd      s                   like(cft1ditd)
003900130128     D s_cft1are       s                   like(cft1are)
004000130128     D s_cft1ared      s                   like(cft1ared)
004100130128     D s_cft1fil       s                   like(cft1fil)
004200130128     D s_cft1fild      s                   like(cft1fild)
004300130128     D s_cft1age       s                   like(cft1age)
004400130128     D s_cft1aged      s                   like(cft1aged)
004500130128     D s_cft1rgf       s                   like(cft1rgf)
004600130128     D s_cft1rgfd      s                   like(cft1rgfd)
004700130128     D s_cft1clv       s                   like(cft1clv)
004800050608
004900050614     C     *entry        plist
005000050614     C                   parm                    kpjba
005100050614     c                   movel     kpjbu         param
005200050608
005300130128     C     *loval        setll     wfcft11l
005400050608     C                   do        *hival
005500130128     C                   read      wfcft11l
005600130128     C                   if        %eof(wfcft11l)
005700050608     C                   leave
005800050608     C                   endif
005900130128     c* a rottura cliente unificante memorizzo dati nel file
006000130128     c                   if        cft1kun <> s_cft1kun
006100130128     c                   if        s_cft1kun > 0
006200050608     c                   exsr      sr_wrtcft
006300050608     c                   endif
006400130128     c                   z-add     cft1kun       s_cft1kun
006500130128     c                   move      cft1kund      s_cft1kund
006600130128     c                   move      cft1dit       s_cft1dit
006700130128     c                   move      cft1ditd      s_cft1ditd
006800130128     c                   move      cft1are       s_cft1are
006900130128     c                   move      cft1ared      s_cft1ared
007000130128     c                   move      cft1fil       s_cft1fil
007100130128     c                   move      cft1fild      s_cft1fild
007200130128     c                   move      cft1age       s_cft1age
007300130128     c                   move      cft1aged      s_cft1aged
007400130128     c                   move      cft1rgf       s_cft1rgf
007500130128     c                   move      cft1rgfd      s_cft1rgfd
007600130128     c                   move      cft1clv       s_cft1clv
007700050608     c* azzero contatori
007800130128     c                   clear                   wtos
007900130128     c                   clear                   wnspsaldi
008000130128     c                   clear                   wtop
008100130128     c                   clear                   wtoc
008200130114     c                   clear                   wistatp
008300130114     c                   clear                   wistatc
008400130114     c                   clear                   wfuelp
008500130114     c                   clear                   wfuelc
008600130128     c                   clear                   wimpistat
008700130128     c                   clear                   wimpfuel
008800050608     c                   endif
008900050608     c
009000050608     c* INCREMENTO CONTATORI
009100050608     c* numero spedizioni elaborate
009200130128     c                   add       cft1tos       wtos
009300130128     c                   add       cft1nsp       wnspsaldi
009400130128     c                   add       cft1top       wtop
009500130128     c                   add       cft1toc       wtoc
009600130114      *sommo il fuel e istat ricalcolati
009700130128     c                   add       cft1istatp    wistatp
009800130128     c                   add       cft1istatc    wistatc
009900130128     c                   add       cft1fuelp     wfuelp
010000130128     c                   add       cft1fuelc     wfuelc
010100130128      *sommo imponibili fuel e istat ricalcolati
010200130128     c                   add       cft1impist    wimpistat
010300130128     c                   add       cft1impfue    wimpfuel
010400130114     c
010500050608     c                   enddo
010600050609     c* memorizzo ultimi dati nel file
010700050608     c                   exsr      sr_wrtcft
010800050608     c*
010900050608     c                   seton                                        lr
011000050608
011100050608      **-------------------------------------------------------------**
011200050608      ** Scrittura file                                              **
011300050608      **-------------------------------------------------------------**
011400050608     c     sr_wrtcft     BEGSR
011500130128     c                   clear                   wfcft300
011600130128     c                   movel     pilota        cft3per
011700130128     c                   movel     paranno       cft3aae
011800130128     c                   movel     parmese       cft3mme
011900130128     c                   z-add     s_cft1kun     cft3kun
012000130128     c                   move      s_cft1kund    cft3kund
012100130128     c                   move      s_cft1dit     cft3dit
012200130128     c                   move      s_cft1ditd    cft3ditd
012300130128     c                   move      s_cft1are     cft3are
012400130128     c                   move      s_cft1ared    cft3ared
012500130128     c                   move      s_cft1fil     cft3fil
012600130128     c                   move      s_cft1fild    cft3fild
012700130128     c                   move      s_cft1age     cft3age
012800130128     c                   move      s_cft1aged    cft3aged
012900130128     c                   move      s_cft1rgf     cft3rgf
013000130128     c                   move      s_cft1rgfd    cft3rgfd
013100130128     c                   move      s_cft1clv     cft3clv
013200130128     c                   z-add     wtop          cft3top
013300130128     c                   z-add     wtoc          cft3toc
013400130128     c                   z-add     wtos          cft3tos
013500130128     c                   z-add     wnspsaldi     cft3nsp
013600130128     c                   z-add     wistatp       cft3istatp
013700130128     c                   z-add     wistatc       cft3istatc
013800130128     c                   z-add     wfuelp        cft3fuelp
013900130128     c                   z-add     wfuelc        cft3fuelc
014000130128     c                   z-add     wimpistat     cft3impist
014100130128     c                   z-add     wimpfuel      cft3impfue
014200130114
014300130128     c                   write     wfcft300
014400050608     c                   endsr
