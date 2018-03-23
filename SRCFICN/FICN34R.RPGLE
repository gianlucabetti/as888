000100990504     H* ACTGRP=*CALLER
000200151016     H DEBUG DECEDIT('0,') DATEDIT(*DMY/) option(*nodebugio)
000300020611     fazorg02l  if   e           k disk
000400020611     ffifss01l  if   e           k disk
000500030115     ffifsc01l  if   e           k disk
000600030522     ffifsa01l  if   e           k disk
000700030522     ffifre02l  if   e           k disk
000800030922     ffifsr00f  o    e             disk    usropn
000900020619     ftabel00f  if   e           k disk
001000030225     ftntbe01l  if   e           k disk
001100170419     fAZQLK01L  uf a e           k disk
001200020619     fecoprt1   o    f  198        printer infds(stamp1) usropn
001300020619     fecoprt2   o    f  198        printer infds(stamp2) usropn
001400020619     fecoprt3   o    f  198        printer infds(stamp3) usropn
001500030508     fecoprt4   o    f  198        printer infds(stamp4) usropn
001600020612     d
001700020612     d kpjba         e ds
001800020619     d ds17          e ds
001900020619     d ds05          e ds
002000030225     d dccg          e ds
002100020612
002200020612     d param           ds
002300020612     d  dalladat                      8s 0
002400020612     d  alladat                       8s 0
002500020625     d  dist                          1    dim(10)
002600030403     d  Dovr                          1
002700050506     d  frigen                        1
002800050506     d  fudate                        8s 0
002900051123     d  riepcdg                       1
003000151014      * parametri chiamata reperimento nuove stime
003100151014     d ficn34s         ds
003200151014     d  pvocA                         8    dim(10)
003300151014     d  pvocD                         8    dim(10)
003400151014     d  pdata                          d
003500151014     d  pfil                          3
003600151014     d  ptsr                          1
003700151014     d  pimpo                        13s 3
003800151014      *
003900151014      * parametri chiamata reperimento voci stime di sede
004000151014     d ficn34v         ds
004100151014     d  pDave                         8    dim(10)
004200151014     d  pDdar                         8    dim(10)
004300151014     d  pAave                         8    dim(10)
004400151014     d  pAdar                         8    dim(10)
004500151014     d  pcave                         8    dim(10)
004600151014     d  pcdar                         8    dim(10)
004700151014      *
004800151014     d  Dave           s              8    dim(10)
004900151014     d  Ddar           s              8    dim(10)
005000151014     d  Aave           s              8    dim(10)
005100151014     d  Adar           s              8    dim(10)
005200151014     d  Cave           s              8    dim(10)
005300151014     d  Cdar           s              8    dim(10)
005400151014     d  vuoto          s              8
005500020612
005600020521     d rekok           s              1
005700020611     d exdistr         s                   like(orgfl3)
005800020611     d exarea          s                   like(orgcar)
005900020611     d exfil           s                   like(orgfil)
006000030527     d exfssddc        s                   like(fssddc)
006100030515      *campi totale per stime giornaliero autotrasportatori
006200030515     d totstm1d        s                   like(totcos1d)
006300030515     d totstm2d        s                   like(totcos1d)
006400030515     d totstm3d        s                   like(totcos1d)
006500030515     d totstm4d        s                   like(totcos1d)
006600030515      *campi totale per stime periodo   autotrasportatori
006700030515     d totstm1p        s                   like(totcos1d)
006800030515     d totstm2p        s                   like(totcos1d)
006900030515     d totstm3p        s                   like(totcos1d)
007000030515     d totstm4p        s                   like(totcos1d)
007100030515      *totale per stime giornaliero stampa autotrasportatori
007200030515     d totstm1ds       s                   like(totcos1ds)
007300030515     d totstm2ds       s                   like(totcos1ds)
007400030515     d totstm3ds       s                   like(totcos1ds)
007500030515     d totstm4ds       s                   like(totcos1ds)
007600030515      *totale per stime periodo stampa   autotrasportatori
007700030515     d totstm1ps       s                   like(totcos1ds)
007800030515     d totstm2ps       s                   like(totcos1ds)
007900030515     d totstm3ps       s                   like(totcos1ds)
008000030515     d totstm4ps       s                   like(totcos1ds)
008100030515      *campi totale per stime giornaliero coop
008200030515     d totstm1dx       s                   like(totcos1d)
008300030515     d totstm2dx       s                   like(totcos1d)
008400030515     d totstm3dx       s                   like(totcos1d)
008500030515     d totstm4dx       s                   like(totcos1d)
008600030515      *campi totale per stime periodo   coop
008700030515     d totstm1px       s                   like(totcos1d)
008800030515     d totstm2px       s                   like(totcos1d)
008900030515     d totstm3px       s                   like(totcos1d)
009000030515     d totstm4px       s                   like(totcos1d)
009100030515      *totale per stime giornaliero stampa coop
009200030515     d totstm1dsx      s                   like(totcos1ds)
009300030515     d totstm2dsx      s                   like(totcos1ds)
009400030515     d totstm3dsx      s                   like(totcos1ds)
009500030515     d totstm4dsx      s                   like(totcos1ds)
009600030515      *totale per stime periodo stampa coop
009700030515     d totstm1psx      s                   like(totcos1ds)
009800030515     d totstm2psx      s                   like(totcos1ds)
009900030515     d totstm3psx      s                   like(totcos1ds)
010000030515     d totstm4psx      s                   like(totcos1ds)
010100030515      *campi totale per stime giornaliero aff/def
010200030515     d totstm1da       s                   like(totcos1d)
010300030515     d totstm2da       s                   like(totcos1d)
010400030515     d totstm3da       s                   like(totcos1d)
010500030515     d totstm4da       s                   like(totcos1d)
010600030515      *campi totale per stime periodo   aff/def
010700030515     d totstm1pa       s                   like(totcos1d)
010800030515     d totstm2pa       s                   like(totcos1d)
010900030515     d totstm3pa       s                   like(totcos1d)
011000030515     d totstm4pa       s                   like(totcos1d)
011100030515      *totale per stime giornaliero stampa aff/def
011200030515     d totstm1dsa      s                   like(totcos1ds)
011300030515     d totstm2dsa      s                   like(totcos1ds)
011400030515     d totstm3dsa      s                   like(totcos1ds)
011500030515     d totstm4dsa      s                   like(totcos1ds)
011600030515      *totale per stime periodo stampa aff/def
011700030515     d totstm1psa      s                   like(totcos1ds)
011800030515     d totstm2psa      s                   like(totcos1ds)
011900030515     d totstm3psa      s                   like(totcos1ds)
012000030515     d totstm4psa      s                   like(totcos1ds)
012100030115      *CAMPI PER TOTALIZZAZIONE COSTI autotrasportatori
012200020619      *totali costi giorno
012300020625     d totcos1d        s             11  3
012400020620     d totcos2d        s                   like(totcos1d)
012500020620     d totcos3d        s                   like(totcos1d)
012600020620     d totcos4d        s                   like(totcos1d)
012700020620      *totali competenze giorno
012800020620     d totcom1d        s                   like(totcos1d)
012900020620     d totcom2d        s                   like(totcos1d)
013000020620     d totcom3d        s                   like(totcos1d)
013100020620     d totcom4d        s                   like(totcos1d)
013200020619      *totali costi periodo
013300020620     d totcos1p        s                   like(totcos1d)
013400020620     d totcos2p        s                   like(totcos1d)
013500020620     d totcos3p        s                   like(totcos1d)
013600020620     d totcos4p        s                   like(totcos1d)
013700020620      *totali competenze periodo
013800020620     d totcom1p        s                   like(totcos1d)
013900020620     d totcom2p        s                   like(totcos1d)
014000020620     d totcom3p        s                   like(totcos1d)
014100020620     d totcom4p        s                   like(totcos1d)
014200020620      *totali costi giorno solo conse
014300020620     d totcon1d        s                   like(totcos1d)
014400020620     d totcon2d        s                   like(totcos1d)
014500020620     d totcon3d        s                   like(totcos1d)
014600020620     d totcon4d        s                   like(totcos1d)
014700020620      *totali costi giorno solo ritir
014800020620     d totrit1d        s                   like(totcos1d)
014900020620     d totrit2d        s                   like(totcos1d)
015000020620     d totrit3d        s                   like(totcos1d)
015100020620     d totrit4d        s                   like(totcos1d)
015200020620      *totali costi periodo solo cons
015300020620     d totcon1p        s                   like(totcos1d)
015400020620     d totcon2p        s                   like(totcos1d)
015500020620     d totcon3p        s                   like(totcos1d)
015600020620     d totcon4p        s                   like(totcos1d)
015700020620      *totali costi periodo solo riti
015800020620     d totrit1p        s                   like(totcos1d)
015900020620     d totrit2p        s                   like(totcos1d)
016000020620     d totrit3p        s                   like(totcos1d)
016100020620     d totrit4p        s                   like(totcos1d)
016200020619      *totali competenze giorno solo consegne
016300020620     d totcon1dc       s                   like(totcos1d)
016400020620     d totcon2dc       s                   like(totcos1d)
016500020620     d totcon3dc       s                   like(totcos1d)
016600020620     d totcon4dc       s                   like(totcos1d)
016700020620      *totali competenze giorno solo
016800020620     d totrit1dc       s                   like(totcos1d)
016900020620     d totrit2dc       s                   like(totcos1d)
017000020620     d totrit3dc       s                   like(totcos1d)
017100020620     d totrit4dc       s                   like(totcos1d)
017200020620      *totali competenze periodo solo
017300020620     d totcon1pc       s                   like(totcos1d)
017400020620     d totcon2pc       s                   like(totcos1d)
017500020620     d totcon3pc       s                   like(totcos1d)
017600020620     d totcon4pc       s                   like(totcos1d)
017700020619      *totali competenze periodo solo ritiri
017800020620     d totrit1pc       s                   like(totcos1d)
017900020620     d totrit2pc       s                   like(totcos1d)
018000020620     d totrit3pc       s                   like(totcos1d)
018100020620     d totrit4pc       s                   like(totcos1d)
018200030115      *CAMPI  DA ESPORRE IN STAMPA autotrasportatori
018300020625      *totali costi giorno
018400020625     d totcos1ds       s              8  0
018500020625     d totcos2ds       s                   like(totcos1ds)
018600020625     d totcos3ds       s                   like(totcos1ds)
018700020625     d totcos4ds       s                   like(totcos1ds)
018800020625      *totali competenze giorno
018900020625     d totcom1ds       s                   like(totcos1ds)
019000020625     d totcom2ds       s                   like(totcos1ds)
019100020625     d totcom3ds       s                   like(totcos1ds)
019200020625     d totcom4ds       s                   like(totcos1ds)
019300020625      *totali costi periodo
019400020625     d totcos1ps       s                   like(totcos1ds)
019500020625     d totcos2ps       s                   like(totcos1ds)
019600020625     d totcos3ps       s                   like(totcos1ds)
019700020625     d totcos4ps       s                   like(totcos1ds)
019800020625      *totali competenze periodo
019900020625     d totcom1ps       s                   like(totcos1ds)
020000020625     d totcom2ps       s                   like(totcos1ds)
020100020625     d totcom3ps       s                   like(totcos1ds)
020200020625     d totcom4ps       s                   like(totcos1ds)
020300020625      *totali costi giorno solo conse
020400020625     d totcon1ds       s                   like(totcos1ds)
020500020625     d totcon2ds       s                   like(totcos1ds)
020600020625     d totcon3ds       s                   like(totcos1ds)
020700020625     d totcon4ds       s                   like(totcos1ds)
020800020625      *totali costi giorno solo ritir
020900020625     d totrit1ds       s                   like(totcos1ds)
021000020625     d totrit2ds       s                   like(totcos1ds)
021100020625     d totrit3ds       s                   like(totcos1ds)
021200020625     d totrit4ds       s                   like(totcos1ds)
021300020625      *totali costi periodo solo cons
021400020625     d totcon1ps       s                   like(totcos1ds)
021500020625     d totcon2ps       s                   like(totcos1ds)
021600020625     d totcon3ps       s                   like(totcos1ds)
021700020625     d totcon4ps       s                   like(totcos1ds)
021800020625      *totali costi periodo solo riti
021900020625     d totrit1ps       s                   like(totcos1ds)
022000020625     d totrit2ps       s                   like(totcos1ds)
022100020625     d totrit3ps       s                   like(totcos1ds)
022200020625     d totrit4ps       s                   like(totcos1ds)
022300020625      *totali competenze giorno solo consegne
022400020625     d totcon1dcs      s                   like(totcos1ds)
022500020625     d totcon2dcs      s                   like(totcos1ds)
022600020625     d totcon3dcs      s                   like(totcos1ds)
022700020625     d totcon4dcs      s                   like(totcos1ds)
022800020625      *totali competenze giorno solo
022900020625     d totrit1dcs      s                   like(totcos1ds)
023000020625     d totrit2dcs      s                   like(totcos1ds)
023100020625     d totrit3dcs      s                   like(totcos1ds)
023200020625     d totrit4dcs      s                   like(totcos1ds)
023300020625      *totali competenze periodo solo
023400030114     d totcon1pcs      s                   like(totcos1ds)
023500020625     d totcon2pcs      s                   like(totcos1ds)
023600020625     d totcon3pcs      s                   like(totcos1ds)
023700020625     d totcon4pcs      s                   like(totcos1ds)
023800020625      *totali competenze periodo solo ritiri
023900020625     d totrit1pcs      s                   like(totcos1ds)
024000020625     d totrit2pcs      s                   like(totcos1ds)
024100020625     d totrit3pcs      s                   like(totcos1ds)
024200020625     d totrit4pcs      s                   like(totcos1ds)
024300020625      *delta % competenze costi giornata
024400030822     d totper18_2      s             18  2
024500030203     d totper1s        s              6  2
024600020625     d totper2s        s                   like(totper1s)
024700020625     d totper3s        s                   like(totper1s)
024800020625     d totper4s        s                   like(totper1s)
024900020625      *delta % competenze costi giornata
025000020625     d totper1ps       s                   like(totper1s)
025100020625     d totper2ps       s                   like(totper1s)
025200020625     d totper3ps       s                   like(totper1s)
025300020625     d totper4ps       s                   like(totper1s)
025400030516      *differenza competenze costi giornata autotrasportatori
025500030516     d totdif1s        s              8  0
025600030516     d totdif2s        s                   like(totdif1s)
025700030516     d totdif3s        s                   like(totdif1s)
025800030516     d totdif4s        s                   like(totdif1s)
025900030516      *differenza competenze costi giornata autotrasportatori
026000030516     d totdif1ps       s                   like(totdif1s)
026100030516     d totdif2ps       s                   like(totdif1s)
026200030516     d totdif3ps       s                   like(totdif1s)
026300030516     d totdif4ps       s                   like(totdif1s)
026400030516      *stimati e reali costi giornata autotrasportatori
026500030516     d totsco1s        s                   like(totdif1s)
026600030516     d totsco2s        s                   like(totdif1s)
026700030516     d totsco3s        s                   like(totdif1s)
026800030516     d totsco4s        s                   like(totdif1s)
026900030516      *stimati e reali costi giornata autotrasportatori
027000030516     d totsco1ps       s                   like(totdif1s)
027100030516     d totsco2ps       s                   like(totdif1s)
027200030516     d totsco3ps       s                   like(totdif1s)
027300030516     d totsco4ps       s                   like(totdif1s)
027400030115      * parziali per calcoli autotrasportatori
027500020620     d cosparz         s             15  5
027600020620     d comparz         s             15  5
027700020620     d conparz         s             15  5
027800020620     d ritparz         s             15  5
027900020620     d conparzc        s             15  5
028000020620     d ritparzc        s             15  5
028100030115      *CAMPI PER TOTALIZZAZIONE COSTI Coop
028200030115      *totali costi giorno
028300030115     d totcos1dx       s                   like(totcos1d)
028400030115     d totcos2dx       s                   like(totcos1d)
028500030115     d totcos3dx       s                   like(totcos1d)
028600030115     d totcos4dx       s                   like(totcos1d)
028700030115      *totali competenze giorno
028800030115     d totcom1dx       s                   like(totcos1d)
028900030115     d totcom2dx       s                   like(totcos1d)
029000030115     d totcom3dx       s                   like(totcos1d)
029100030115     d totcom4dx       s                   like(totcos1d)
029200030115      *totali costi periodo
029300030115     d totcos1px       s                   like(totcos1d)
029400030115     d totcos2px       s                   like(totcos1d)
029500030115     d totcos3px       s                   like(totcos1d)
029600030115     d totcos4px       s                   like(totcos1d)
029700030115      *totali competenze periodo
029800030115     d totcom1px       s                   like(totcos1d)
029900030115     d totcom2px       s                   like(totcos1d)
030000030115     d totcom3px       s                   like(totcos1d)
030100030115     d totcom4px       s                   like(totcos1d)
030200030115      *totali costi giorno solo conse
030300030115     d totcon1dx       s                   like(totcos1d)
030400030115     d totcon2dx       s                   like(totcos1d)
030500030115     d totcon3dx       s                   like(totcos1d)
030600030115     d totcon4dx       s                   like(totcos1d)
030700030115      *totali costi giorno solo ritir
030800030115     d totrit1dx       s                   like(totcos1d)
030900030115     d totrit2dx       s                   like(totcos1d)
031000030115     d totrit3dx       s                   like(totcos1d)
031100030115     d totrit4dx       s                   like(totcos1d)
031200030115      *totali costi periodo solo cons
031300030115     d totcon1px       s                   like(totcos1d)
031400030115     d totcon2px       s                   like(totcos1d)
031500030115     d totcon3px       s                   like(totcos1d)
031600030115     d totcon4px       s                   like(totcos1d)
031700030115      *totali costi periodo solo riti
031800030115     d totrit1px       s                   like(totcos1d)
031900030115     d totrit2px       s                   like(totcos1d)
032000030115     d totrit3px       s                   like(totcos1d)
032100030115     d totrit4px       s                   like(totcos1d)
032200030115      *totali competenze giorno solo consegne
032300030115     d totcon1dcx      s                   like(totcos1d)
032400030115     d totcon2dcx      s                   like(totcos1d)
032500030115     d totcon3dcx      s                   like(totcos1d)
032600030115     d totcon4dcx      s                   like(totcos1d)
032700030115      *totali competenze giorno solo
032800030115     d totrit1dcx      s                   like(totcos1d)
032900030115     d totrit2dcx      s                   like(totcos1d)
033000030115     d totrit3dcx      s                   like(totcos1d)
033100030115     d totrit4dcx      s                   like(totcos1d)
033200030115      *totali competenze periodo solo
033300030115     d totcon1pcx      s                   like(totcos1d)
033400030115     d totcon2pcx      s                   like(totcos1d)
033500030115     d totcon3pcx      s                   like(totcos1d)
033600030115     d totcon4pcx      s                   like(totcos1d)
033700030115      *totali competenze periodo solo ritiri
033800030115     d totrit1pcx      s                   like(totcos1d)
033900030115     d totrit2pcx      s                   like(totcos1d)
034000030115     d totrit3pcx      s                   like(totcos1d)
034100030115     d totrit4pcx      s                   like(totcos1d)
034200030115      *CAMPI  DA ESPORRE IN STAMPA Coop
034300030115      *totali costi giorno
034400030115     d totcos1dsx      s                   like(totcos1ds)
034500030115     d totcos2dsx      s                   like(totcos1ds)
034600030115     d totcos3dsx      s                   like(totcos1ds)
034700030115     d totcos4dsx      s                   like(totcos1ds)
034800030115      *totali competenze giorno
034900030115     d totcom1dsx      s                   like(totcos1ds)
035000030115     d totcom2dsx      s                   like(totcos1ds)
035100030115     d totcom3dsx      s                   like(totcos1ds)
035200030115     d totcom4dsx      s                   like(totcos1ds)
035300030115      *totali costi periodo
035400030115     d totcos1psx      s                   like(totcos1ds)
035500030115     d totcos2psx      s                   like(totcos1ds)
035600030115     d totcos3psx      s                   like(totcos1ds)
035700030115     d totcos4psx      s                   like(totcos1ds)
035800030115      *totali competenze periodo
035900030115     d totcom1psx      s                   like(totcos1ds)
036000030115     d totcom2psx      s                   like(totcos1ds)
036100030115     d totcom3psx      s                   like(totcos1ds)
036200030115     d totcom4psx      s                   like(totcos1ds)
036300030115      *totali costi giorno solo conse
036400030115     d totcon1dsx      s                   like(totcos1ds)
036500030115     d totcon2dsx      s                   like(totcos1ds)
036600030115     d totcon3dsx      s                   like(totcos1ds)
036700030115     d totcon4dsx      s                   like(totcos1ds)
036800030115      *totali costi giorno solo ritir
036900030115     d totrit1dsx      s                   like(totcos1ds)
037000030115     d totrit2dsx      s                   like(totcos1ds)
037100030115     d totrit3dsx      s                   like(totcos1ds)
037200030115     d totrit4dsx      s                   like(totcos1ds)
037300030115      *totali costi periodo solo cons
037400030115     d totcon1psx      s                   like(totcos1ds)
037500030115     d totcon2psx      s                   like(totcos1ds)
037600030115     d totcon3psx      s                   like(totcos1ds)
037700030115     d totcon4psx      s                   like(totcos1ds)
037800030115      *totali costi periodo solo riti
037900030115     d totrit1psx      s                   like(totcos1ds)
038000030115     d totrit2psx      s                   like(totcos1ds)
038100030115     d totrit3psx      s                   like(totcos1ds)
038200030115     d totrit4psx      s                   like(totcos1ds)
038300030115      *totali competenze giorno solo consegne
038400030115     d totcon1dcsx     s                   like(totcos1ds)
038500030115     d totcon2dcsx     s                   like(totcos1ds)
038600030115     d totcon3dcsx     s                   like(totcos1ds)
038700030115     d totcon4dcsx     s                   like(totcos1ds)
038800030115      *totali competenze giorno solo
038900030115     d totrit1dcsx     s                   like(totcos1ds)
039000030115     d totrit2dcsx     s                   like(totcos1ds)
039100030115     d totrit3dcsx     s                   like(totcos1ds)
039200030115     d totrit4dcsx     s                   like(totcos1ds)
039300030115      *totali competenze periodo solo
039400030115     d totcon1pcsx     s                   like(totcos1ds)
039500030115     d totcon2pcsx     s                   like(totcos1ds)
039600030115     d totcon3pcsx     s                   like(totcos1ds)
039700030115     d totcon4pcsx     s                   like(totcos1ds)
039800030115      *totali competenze periodo solo ritiri
039900030115     d totrit1pcsx     s                   like(totcos1ds)
040000030115     d totrit2pcsx     s                   like(totcos1ds)
040100030115     d totrit3pcsx     s                   like(totcos1ds)
040200030115     d totrit4pcsx     s                   like(totcos1ds)
040300030115      *delta % competenze costi giornata
040400030115     d totper1sx       s                   like(totper1s)
040500030115     d totper2sx       s                   like(totper1s)
040600030115     d totper3sx       s                   like(totper1s)
040700030115     d totper4sx       s                   like(totper1s)
040800030115      *delta % competenze costi giornata
040900030115     d totper1psx      s                   like(totper1s)
041000030115     d totper2psx      s                   like(totper1s)
041100030115     d totper3psx      s                   like(totper1s)
041200030115     d totper4psx      s                   like(totper1s)
041300030516      *differenza competenze costi giornata coop
041400030516     d totdif1sx       s                   like(totdif1s)
041500030516     d totdif2sx       s                   like(totdif1s)
041600030516     d totdif3sx       s                   like(totdif1s)
041700030516     d totdif4sx       s                   like(totdif1s)
041800030516      *differenza competenze costi giornata coop
041900030516     d totdif1psx      s                   like(totdif1s)
042000030516     d totdif2psx      s                   like(totdif1s)
042100030516     d totdif3psx      s                   like(totdif1s)
042200030516     d totdif4psx      s                   like(totdif1s)
042300030516      *stimati e reali costi giornata coop
042400030516     d totsco1sx       s                   like(totdif1s)
042500030516     d totsco2sx       s                   like(totdif1s)
042600030516     d totsco3sx       s                   like(totdif1s)
042700030516     d totsco4sx       s                   like(totdif1s)
042800030516      *stimati e reali costi giornata coop
042900030516     d totsco1psx      s                   like(totdif1s)
043000030516     d totsco2psx      s                   like(totdif1s)
043100030516     d totsco3psx      s                   like(totdif1s)
043200030516     d totsco4psx      s                   like(totdif1s)
043300030115      * parziali per calcoli Coop
043400030115     d cosparzx        s             15  5
043500030115     d comparzx        s             15  5
043600030115     d conparzx        s             15  5
043700030523     d ritparzx        s             15  5
043800030115     d conparzcx       s             15  5
043900030115     d ritparzcx       s             15  5
044000030115
044100030515      *CAMPI PER TOTALIZZAZIONE COSTI affluenze defluenze
044200030515      *totali costi giorno
044300030515     d totcos1da       s                   like(totcos1d)
044400030515     d totcos2da       s                   like(totcos1d)
044500030515     d totcos3da       s                   like(totcos1d)
044600030515     d totcos4da       s                   like(totcos1d)
044700030515      *totali competenze giorno
044800030515     d totcom1da       s                   like(totcos1d)
044900030515     d totcom2da       s                   like(totcos1d)
045000030515     d totcom3da       s                   like(totcos1d)
045100030515     d totcom4da       s                   like(totcos1d)
045200030515      *totali costi periodo
045300030515     d totcos1pa       s                   like(totcos1d)
045400030515     d totcos2pa       s                   like(totcos1d)
045500030515     d totcos3pa       s                   like(totcos1d)
045600030515     d totcos4pa       s                   like(totcos1d)
045700030515      *totali competenze periodo
045800030515     d totcom1pa       s                   like(totcos1d)
045900030515     d totcom2pa       s                   like(totcos1d)
046000030515     d totcom3pa       s                   like(totcos1d)
046100030515     d totcom4pa       s                   like(totcos1d)
046200030515      *totali costi giorno solo conse
046300030515     d totcon1da       s                   like(totcos1d)
046400030515     d totcon2da       s                   like(totcos1d)
046500030515     d totcon3da       s                   like(totcos1d)
046600030515     d totcon4da       s                   like(totcos1d)
046700030515      *totali costi giorno solo ritir
046800030515     d totrit1da       s                   like(totcos1d)
046900030515     d totrit2da       s                   like(totcos1d)
047000030515     d totrit3da       s                   like(totcos1d)
047100030515     d totrit4da       s                   like(totcos1d)
047200030515      *totali costi periodo solo cons
047300030515     d totcon1pa       s                   like(totcos1d)
047400030515     d totcon2pa       s                   like(totcos1d)
047500030515     d totcon3pa       s                   like(totcos1d)
047600030515     d totcon4pa       s                   like(totcos1d)
047700030515      *totali costi periodo solo riti
047800030515     d totrit1pa       s                   like(totcos1d)
047900030515     d totrit2pa       s                   like(totcos1d)
048000030515     d totrit3pa       s                   like(totcos1d)
048100030515     d totrit4pa       s                   like(totcos1d)
048200030515      *totali competenze giorno solo consegne
048300030515     d totcon1dca      s                   like(totcos1d)
048400030515     d totcon2dca      s                   like(totcos1d)
048500030515     d totcon3dca      s                   like(totcos1d)
048600030515     d totcon4dca      s                   like(totcos1d)
048700030515      *totali competenze giorno solo
048800030515     d totrit1dca      s                   like(totcos1d)
048900030515     d totrit2dca      s                   like(totcos1d)
049000030515     d totrit3dca      s                   like(totcos1d)
049100030515     d totrit4dca      s                   like(totcos1d)
049200030515      *totali competenze periodo solo
049300030515     d totcon1pca      s                   like(totcos1d)
049400030515     d totcon2pca      s                   like(totcos1d)
049500030515     d totcon3pca      s                   like(totcos1d)
049600030515     d totcon4pca      s                   like(totcos1d)
049700030515      *totali competenze periodo solo ritiri
049800030515     d totrit1pca      s                   like(totcos1d)
049900030515     d totrit2pca      s                   like(totcos1d)
050000030515     d totrit3pca      s                   like(totcos1d)
050100030515     d totrit4pca      s                   like(totcos1d)
050200030522      *CAMPI  DA ESPORRE IN STAMPA aff/def
050300030515      *totali costi giorno
050400030515     d totcos1dsa      s                   like(totcos1ds)
050500030515     d totcos2dsa      s                   like(totcos1ds)
050600030516     d totcos3dsa      s                   like(totcos1ds)
050700030515     d totcos4dsa      s                   like(totcos1ds)
050800030515      *totali competenze giorno
050900030515     d totcom1dsa      s                   like(totcos1ds)
051000030515     d totcom2dsa      s                   like(totcos1ds)
051100030515     d totcom3dsa      s                   like(totcos1ds)
051200030515     d totcom4dsa      s                   like(totcos1ds)
051300030515      *totali costi periodo
051400030515     d totcos1psa      s                   like(totcos1ds)
051500030515     d totcos2psa      s                   like(totcos1ds)
051600030515     d totcos3psa      s                   like(totcos1ds)
051700030515     d totcos4psa      s                   like(totcos1ds)
051800030515      *totali competenze periodo
051900030515     d totcom1psa      s                   like(totcos1ds)
052000030515     d totcom2psa      s                   like(totcos1ds)
052100030515     d totcom3psa      s                   like(totcos1ds)
052200030515     d totcom4psa      s                   like(totcos1ds)
052300030515      *totali costi giorno solo conse
052400030515     d totcon1dsa      s                   like(totcos1ds)
052500030515     d totcon2dsa      s                   like(totcos1ds)
052600030515     d totcon3dsa      s                   like(totcos1ds)
052700030515     d totcon4dsa      s                   like(totcos1ds)
052800030515      *totali costi giorno solo ritir
052900030515     d totrit1dsa      s                   like(totcos1ds)
053000030515     d totrit2dsa      s                   like(totcos1ds)
053100030515     d totrit3dsa      s                   like(totcos1ds)
053200030515     d totrit4dsa      s                   like(totcos1ds)
053300030515      *totali costi periodo solo cons
053400030515     d totcon1psa      s                   like(totcos1ds)
053500030515     d totcon2psa      s                   like(totcos1ds)
053600030515     d totcon3psa      s                   like(totcos1ds)
053700030515     d totcon4psa      s                   like(totcos1ds)
053800030515      *totali costi periodo solo riti
053900030515     d totrit1psa      s                   like(totcos1ds)
054000030515     d totrit2psa      s                   like(totcos1ds)
054100030515     d totrit3psa      s                   like(totcos1ds)
054200030515     d totrit4psa      s                   like(totcos1ds)
054300030515      *totali competenze giorno solo consegne
054400030515     d totcon1dcsa     s                   like(totcos1ds)
054500030515     d totcon2dcsa     s                   like(totcos1ds)
054600030515     d totcon3dcsa     s                   like(totcos1ds)
054700030515     d totcon4dcsa     s                   like(totcos1ds)
054800030515      *totali competenze giorno solo
054900030515     d totrit1dcsa     s                   like(totcos1ds)
055000030515     d totrit2dcsa     s                   like(totcos1ds)
055100030515     d totrit3dcsa     s                   like(totcos1ds)
055200030515     d totrit4dcsa     s                   like(totcos1ds)
055300030515      *totali competenze periodo solo
055400030515     d totcon1pcsa     s                   like(totcos1ds)
055500030515     d totcon2pcsa     s                   like(totcos1ds)
055600030515     d totcon3pcsa     s                   like(totcos1ds)
055700030515     d totcon4pcsa     s                   like(totcos1ds)
055800030515      *totali competenze periodo solo ritiri
055900030515     d totrit1pcsa     s                   like(totcos1ds)
056000030515     d totrit2pcsa     s                   like(totcos1ds)
056100030515     d totrit3pcsa     s                   like(totcos1ds)
056200030515     d totrit4pcsa     s                   like(totcos1ds)
056300030515      *delta % competenze costi giornata
056400030515     d totper1sa       s                   like(totper1s)
056500030515     d totper2sa       s                   like(totper1s)
056600030515     d totper3sa       s                   like(totper1s)
056700030515     d totper4sa       s                   like(totper1s)
056800030515      *delta % competenze costi giornata
056900030515     d totper1psa      s                   like(totper1s)
057000030515     d totper2psa      s                   like(totper1s)
057100030515     d totper3psa      s                   like(totper1s)
057200030515     d totper4psa      s                   like(totper1s)
057300030516      *differenza competenze costi giornata aff/def
057400030516     d totdif1sa       s                   like(totdif1s)
057500030516     d totdif2sa       s                   like(totdif1s)
057600030516     d totdif3sa       s                   like(totdif1s)
057700030516     d totdif4sa       s                   like(totdif1s)
057800030516      *differenza competenze costi giornata aff/def
057900030516     d totdif1psa      s                   like(totdif1s)
058000030516     d totdif2psa      s                   like(totdif1s)
058100030516     d totdif3psa      s                   like(totdif1s)
058200030516     d totdif4psa      s                   like(totdif1s)
058300030516      *stimati e reali costi giornata aff/def
058400030516     d totsco1sa       s                   like(totdif1s)
058500030516     d totsco2sa       s                   like(totdif1s)
058600030516     d totsco3sa       s                   like(totdif1s)
058700030516     d totsco4sa       s                   like(totdif1s)
058800030516      *stimati e reali costi giornata aff/def
058900030516     d totsco1psa      s                   like(totdif1s)
059000030516     d totsco2psa      s                   like(totdif1s)
059100030516     d totsco3psa      s                   like(totdif1s)
059200030516     d totsco4psa      s                   like(totdif1s)
059300030522      * parziali per calcoli aff/def
059400030515     d cosparza        s             15  5
059500030515     d comparza        s             15  5
059600030515     d conparza        s             15  5
059700030515     d ritparza        s             15  5
059800030515     d conparzca       s             15  5
059900030515     d ritparzca       s             15  5
060000030528      * campi per totali in stampa
060100030528     d stime1          s              8  0
060200030528     d stime2          s                   like(stime1)
060300030528     d stime3          s                   like(stime1)
060400030528     d stime4          s                   like(stime1)
060500030528     d stime1p         s                   like(stime1)
060600030528     d stime2p         s                   like(stime1)
060700030528     d stime3p         s                   like(stime1)
060800030528     d stime4p         s                   like(stime1)
060900030528     d reali1          s                   like(stime1)
061000030528     d reali2          s                   like(stime1)
061100030528     d reali3          s                   like(stime1)
061200030528     d reali4          s                   like(stime1)
061300030528     d reali1p         s                   like(stime1)
061400030528     d reali2p         s                   like(stime1)
061500030528     d reali3p         s                   like(stime1)
061600030528     d reali4p         s                   like(stime1)
061700030528     d costi1          s                   like(stime1)
061800030528     d costi2          s                   like(stime1)
061900030528     d costi3          s                   like(stime1)
062000030528     d costi4          s                   like(stime1)
062100030528     d costi1p         s                   like(stime1)
062200030528     d costi2p         s                   like(stime1)
062300030528     d costi3p         s                   like(stime1)
062400030528     d costi4p         s                   like(stime1)
062500030528     d compe1          s                   like(stime1)
062600030528     d compe2          s                   like(stime1)
062700030528     d compe3          s                   like(stime1)
062800030528     d compe4          s                   like(stime1)
062900030528     d compe1p         s                   like(stime1)
063000030528     d compe2p         s                   like(stime1)
063100030528     d compe3p         s                   like(stime1)
063200030528     d compe4p         s                   like(stime1)
063300030528     d diffe1          s                   like(stime1)
063400030528     d diffe2          s                   like(stime1)
063500030528     d diffe3          s                   like(stime1)
063600030528     d diffe4          s                   like(stime1)
063700030528     d diffe1p         s                   like(stime1)
063800030528     d diffe2p         s                   like(stime1)
063900030528     d diffe3p         s                   like(stime1)
064000030528     d diffe4p         s                   like(stime1)
064100030528     d tpcompe1        s                   like(totper1s)
064200030528     d tpcompe2        s                   like(totper1s)
064300030528     d tpcompe3        s                   like(totper1s)
064400030528     d tpcompe4        s                   like(totper1s)
064500030528     d tpcompe1p       s                   like(totper1s)
064600030528     d tpcompe2p       s                   like(totper1s)
064700030528     d tpcompe3p       s                   like(totper1s)
064800030528     d tpcompe4p       s                   like(totper1s)
064900030515      * parziale per calcolo stime
065000030515     d importo         s             15  5
065100030515
065200030225     d dataiso         s               d   datfmt(*iso)
065300030225     d dataiso2        s               d   datfmt(*iso)
065400030225     d dataiso3        s               d   datfmt(*iso)
065500020619     d dataeur         s               d   datfmt(*eur)
065600030225     d dalladatg       s                   like(dalladat)
065700030225     d alladatg        s                   like(alladat)
065800030225     d alladatx        s                   like(alladat)
065900030115
066000020619     d lvl             s              1  0
066100020619     d livello         s              1  0
066200020619     d PrmOutq         s             10
066300020620     d desfil          s             15
066400030922     d schedulato      s              1
066500020619      *
066600030225     D WLBDAT          DS
066700030225     D  G02DAT                 1      8  0
066800030225     D  G02INV                 9     16  0
066900030225     D  G02ERR                17     17
067000030225     D  G02TGI                18     22  0
067100030225
067200030225     d Wdata8          DS
067300030225     d  dadata                 1      8  0
067400030225     d  adata                  9     16  0
067500030225     d  GioLav                17     21  0
067600030225      *
067700020611     d stamp1          ds
067800020611     d  lin1                 367    368b 0
067900020611     d stamp2          ds
068000020611     d  lin2                 367    368b 0
068100020611     d stamp3          ds
068200020611     d  lin3                 367    368b 0
068300020614     d stamp4          ds
068400020614     d  lin4                 367    368b 0
068500020619      *
068600030922     d CMD             S             62    DIM(5) CTDATA PERRCD(1)              OVRPRTF PRTF
068700030403      *
068800030403     D OutqSede        C                   CONST('QPRINTS   ')
068900000607      *--------------------------------------------------------------------
069000000607     c
069100020611      *loop sui record di azorg
069200020611     c     *loval        setll     azorg02l
069300000607     c                   do        *hival
069400020611     c                   read      azorg02l                               99
069500020517     c   99              leave
069600031001     c*                  if        orgfag <> 'F' and orgfag <> 'A'
069700031001     c                   if        orgfag <> 'F'
069800020620     c                   iter
069900020620     c                   end
070000030307     c                   movel     orgde8        aster             1
070100030307     c                   if        aster = *blank
070200030307     c                   iter
070300030307     c                   end
070400030307     c
070500030226      * se schedulato (senza parametri in kpjbu) non controllo il distretto
070600030226      * ed elaboro tutto
070700030226     c                   if        dista <> *blank
070800020625     c     orgfl3        lookup    dist                                   89
070900020625     c  n89              iter
071000030226     c                   endif
071100020611
071200020620     c                   exsr      main
071300151014      *verifica su tabella VPO se filiale partita nuove stime
071400151014     c                   exsr      repVpo
071500151014      *
071600020611     c                   exsr      elabora
071700020619     c                   enddo
071800020620
071900020620     c                   exsr      main
072000020620
072100030114      * stampa totali per l'azienda
072200020619     c                   if        lin4 > 60
072300020619     c                   except    testa4
072400020619     c                   end
072500030115      * autotrasportatori
072600020625     c                   eval      totcos4ds =   totcos4d
072700020625     c                   eval      totcom4ds =   totcom4d
072800020625     c                   eval      totcos4ps =   totcos4p
072900020625     c                   eval      totcom4ps =   totcom4p
073000020625     c                   eval      totcon4ds =   totcon4d
073100020625     c                   eval      totrit4ds =   totrit4d
073200020625     c                   eval      totcon4ps =   totcon4p
073300020625     c                   eval      totrit4ps =   totrit4p
073400020625     c                   eval      totcon4dcs =  totcon4dc
073500020625     c                   eval      totrit4dcs =  totrit4dc
073600020625     c                   eval      totcon4pcs =  totcon4pc
073700020625     c                   eval      totrit4pcs =  totrit4pc
073800030522
073900030522     c                   eval      totstm4ps =  totstm4p
074000030522     c                   eval      totstm4ds =  totstm4d
074100030522     c                   eval      totsco4s = totstm4d + totcos4d
074200030522     c                   eval      totsco4ps = totstm4p + totcos4p
074300030522     c                   eval      totdif4s = totcom4d - totsco4s
074400030522     c                   eval      totdif4ps = totcom4p - totsco4ps
074500020625
074600030203     c                   if        totcom4d > 0
074700030822     c                   eval      totper18_2 = ((totcom4d - totsco4s) * 100)
074800020625     c                                        / totcom4d
074900030822     c                   Z-ADD     totper18_2    totper4s
075000030822      *
075100030203     c                   setoff                                       7071
075200030203     c     totper4s      comp      999                                70
075300030203     c  n70totper4s      comp      -999                                 71
075400030203     c   70              z-add     999           totper4s
075500030203     c   71              z-add     -999          totper4s
075600030115     c                   else
075700030115     c                   z-add     0             totper4s
075800020625     c                   endif
075900020625
076000030203     c                   if        totcom4p > 0
076100030822     c                   eval      totper18_2 = ((totcom4p - totsco4ps) * 100)
076200020625     c                                        / totcom4p
076300030822     c                   Z-ADD     totper18_2    totper4ps
076400030822      *
076500030203     c                   setoff                                       7071
076600030203     c     totper4ps     comp      999                                70
076700030203     c  n70totper4ps     comp      -999                                 71
076800030203     c   70              z-add     999           totper4ps
076900030203     c   71              z-add     -999          totper4ps
077000030115     c                   else
077100030115     c                   z-add     0             totper4ps
077200020625     c                   endif
077300030115      * Coop
077400030116     c                   eval      totcos4dsx =   totcos4dx
077500030116     c                   eval      totcom4dsx =   totcom4dx
077600030116     c                   eval      totcos4psx =   totcos4px
077700030116     c                   eval      totcom4psx =   totcom4px
077800030116     c                   eval      totcon4dsx =   totcon4dx
077900030116     c                   eval      totrit4dsx =   totrit4dx
078000030116     c                   eval      totcon4psx =   totcon4px
078100030116     c                   eval      totrit4psx =   totrit4px
078200030116     c                   eval      totcon4dcsx =  totcon4dcx
078300030116     c                   eval      totrit4dcsx =  totrit4dcx
078400030116     c                   eval      totcon4pcsx =  totcon4pcx
078500030116     c                   eval      totrit4pcsx =  totrit4pcx
078600030522
078700030522     c                   eval      totstm4psx =  totstm4px
078800030522     c                   eval      totstm4dsx =  totstm4dx
078900030522     c                   eval      totsco4sx = totstm4dx + totcos4dx
079000030522     c                   eval      totsco4psx = totstm4px + totcos4px
079100030522     c                   eval      totdif4sx = totcom4dx - totsco4sx
079200030522     c                   eval      totdif4psx = totcom4px - totsco4psx
079300030522
079400030203     c                   if        totcom4dx > 0
079500030822     c                   eval      totper18_2= ((totcom4dsx - totsco4sx) * 100)
079600030609     c                                        / totcom4dsx
079700030822     c                   Z-ADD     totper18_2    totper4sx
079800030822      *
079900030203     c                   setoff                                       7071
080000030203     c     totper4sx     comp      999                                70
080100030203     c  n70totper4sx     comp      -999                                 71
080200030203     c   70              z-add     999           totper4sx
080300030203     c   71              z-add     -999          totper4sx
080400030116     c                   else
080500030116     c                   z-add     0             totper4sx
080600030116     c                   endif
080700030116
080800030203     c                   if        totcom4px > 0
080900030822     c                   eval      totper18_2 = ((totcom4px - totsco4psx) * 100)
081000030116     c                                        / totcom4px
081100030822     c                   Z-ADD     totper18_2    totper4psx
081200030822      *
081300030203     c                   setoff                                       7071
081400030203     c     totper4psx    comp      999                                70
081500030203     c  n70totper4psx    comp      -999                                 71
081600030203     c   70              z-add     999           totper4psx
081700030203     c   71              z-add     -999          totper4psx
081800030116     c                   else
081900030116     c                   z-add     0             totper4psx
082000030116     c                   endif
082100030116
082200030515      * Affluenze/defluenze
082300030515     c                   eval      totcos4dsa =   totcos4da
082400030515     c                   eval      totcom4dsa =   totcom4da
082500030515     c                   eval      totcos4psa =   totcos4pa
082600030515     c                   eval      totcom4psa =   totcom4pa
082700030515     c                   eval      totcon4dsa =   totcon4da
082800030515     c                   eval      totrit4dsa =   totrit4da
082900030515     c                   eval      totcon4psa =   totcon4pa
083000030515     c                   eval      totrit4psa =   totrit4pa
083100030515     c                   eval      totcon4dcsa =  totcon4dca
083200030515     c                   eval      totrit4dcsa =  totrit4dca
083300030515     c                   eval      totcon4pcsa =  totcon4pca
083400030515     c                   eval      totrit4pcsa =  totrit4pca
083500030522
083600030522     c                   eval      totstm4psa =  totstm4pa
083700030522     c                   eval      totstm4dsa =  totstm4da
083800030522     c                   eval      totsco4sa = totstm4da + totcos4da
083900030522     c                   eval      totsco4psa = totstm4pa + totcos4pa
084000030522     c                   eval      totdif4sa = totcom4da - totsco4sa
084100030522     c                   eval      totdif4psa = totcom4pa - totsco4psa
084200030515
084300030515     c                   if        totcom4da > 0
084400030822     c                   eval      totper18_2= ((totcom4dsa - totsco4sa) * 100)
084500030609     c                                        / totcom4dsa
084600030822     c                   Z-ADD     totper18_2    totper4sa
084700030822      *
084800030515     c                   setoff                                       7071
084900030515     c     totper4sa     comp      999                                70
085000030515     c  n70totper4sa     comp      -999                                 71
085100030515     c   70              z-add     999           totper4sa
085200030515     c   71              z-add     -999          totper4sa
085300030515     c                   else
085400030515     c                   z-add     0             totper4sa
085500030515     c                   endif
085600030515
085700030515     c                   if        totcom4pa > 0
085800030822     c                   eval      totper18_2 = ((totcom4pa - totsco4psa) * 100)
085900030515     c                                        / totcom4pa
086000030822     c                   Z-ADD     totper18_2    totper4psa
086100030822      *
086200030515     c                   setoff                                       7071
086300030515     c     totper4psa    comp      999                                70
086400030515     c  n70totper4psa    comp      -999                                 71
086500030515     c   70              z-add     999           totper4psa
086600030515     c   71              z-add     -999          totper4psa
086700030515     c                   else
086800030515     c                   z-add     0             totper4psa
086900030515     c                   endif
087000030528      * ricavo i totali per azienda
087100030528      *giornalieri
087200030528     c                   eval      stime4 = totstm4ds + totstm4dsx + totstm4dsa
087300030528     c                   eval      reali4 = totcos4ds + totcos4dsx + totcos4dsa
087400030528     c                   eval      costi4 = totsco4s + totsco4sx + totsco4sa
087500030528     c                   eval      compe4 = totcom4ds + totcom4dsx + totcom4dsa
087600030528     c                   eval      diffe4 = totdif4s + totdif4sx + totdif4sa
087700030528
087800030528      *periodo
087900030528     c                   eval      stime4p= totstm4ps + totstm4psx + totstm4psa
088000030528     c                   eval      reali4p= totcos4ps + totcos4psx + totcos4psa
088100030528     c                   eval      costi4p= totsco4ps + totsco4psx + totsco4psa
088200030528     c                   eval      compe4p= totcom4ps + totcom4psx + totcom4psa
088300030528     c                   eval      diffe4p= totdif4ps + totdif4psx + totdif4psa
088400030528      *calcola le percentuali
088500030528     c                   if        compe4  > 0
088600030822     c                   eval      totper18_2 = ((compe4 -costi4) * 100) /compe4
088700030822     c                   Z-ADD     totper18_2    tpcompe4
088800030822      *
088900030528     c                   setoff                                       7071
089000030528     c     tpcompe4      comp      999                                70
089100030528     c  n70tpcompe4      comp      -999                                 71
089200030528     c   70              z-add     999           tpcompe4
089300030528     c   71              z-add     -999          tpcompe4
089400030528     c                   else
089500030528     c                   z-add     0             tpcompe4
089600030528     c                   endif
089700030528
089800030528     c                   if        compe4p > 0
089900030822     c                   eval      totper18_2 = ((compe4p-costi4p) * 100)
090000030822     c                                        / compe4p
090100030822     c                   Z-ADD     totper18_2    tpcompe4p
090200030822      *
090300030528     c                   setoff                                       7071
090400030528     c     tpcompe4p     comp      999                                70
090500030528     c  n70tpcompe4p     comp      -999                                 71
090600030528     c   70              z-add     999           tpcompe4p
090700030528     c   71              z-add     -999          tpcompe4p
090800030528     c                   else
090900030528     c                   z-add     0             tpcompe4p
091000030528     c                   endif
091100030515
091200020619     c                   except    azienda
091300020611
091400020521     c                   seton                                        lr
091500020620      *-------------------------------------------------
091600020620     c     main          begsr
091700020620      *------------------------------------------------
091800100210     c                   if        rekok <> *blank
091900020620
092000020620      *rottura filiale
092100020620     c                   if        orgfil <> exfil
092200020620     c                   except    testa1
092300020620     c                   exsr      testate
092400030115      *imposto i  campi x la stampa autotrasportatori
092500020625     c                   eval      totcos1ds =   totcos1d
092600020625     c                   eval      totcom1ds =   totcom1d
092700020625     c                   eval      totcos1ps =   totcos1p
092800020625     c                   eval      totcom1ps =   totcom1p
092900020625     c                   eval      totcon1ds =   totcon1d
093000020625     c                   eval      totrit1ds =   totrit1d
093100020625     c                   eval      totcon1ps =   totcon1p
093200020625     c                   eval      totrit1ps =   totrit1p
093300020625     c                   eval      totcon1dcs =  totcon1dc
093400020625     c                   eval      totrit1dcs =  totrit1dc
093500030114     c                   eval      totcon1pcs =  totcon1pc
093600020625     c                   eval      totrit1pcs =  totrit1pc
093700020625
093800030522     c                   eval      totstm1ps  =  totstm1p
093900030522     c                   eval      totstm1ds  =  totstm1d
094000030522     c                   eval      totsco1s  = totstm1d  + totcos1d
094100030522     c                   eval      totsco1ps  = totstm1p  + totcos1p
094200030522     c                   eval      totdif1s  = totcom1d  - totsco1s
094300030522     c                   eval      totdif1ps  = totcom1p  - totsco1ps
094400030522
094500030203     c                   if        totcom1d > 0
094600030822     c                   eval      totper18_2 = ((totcom1d - totsco1s) * 100)
094700020625     c                                        / totcom1d
094800030822     c                   Z-ADD     totper18_2    totper1s
094900030822      *
095000030203     c                   setoff                                       7071
095100030203     c     totper1s      comp      999                                70
095200030203     c  n70totper1s      comp      -999                                 71
095300030203     c   70              z-add     999           totper1s
095400030203     c   71              z-add     -999          totper1s
095500030116     c                   else
095600030116     c                   z-add     0             totper1s
095700020625     c                   endif
095800020625
095900030203     c                   if        totcom1p > 0
096000030822     c                   eval      totper18_2= ((totcom1p - totsco1ps) * 100)
096100020625     c                                        / totcom1p
096200030822     c                   Z-ADD     totper18_2    totper1ps
096300030822      *
096400030203     c                   setoff                                       7071
096500030203     c     totper1ps     comp      999                                70
096600030203     c  n70totper1ps     comp      -999                                 71
096700030203     c   70              z-add     999           totper1ps
096800030203     c   71              z-add     -999          totper1ps
096900030116     c                   else
097000030116     c                   z-add     0             totper1ps
097100020625     c                   endif
097200030115      *imposto i  campi x la stampa coop
097300030115     c                   eval      totcos1dsx =   totcos1dx
097400030115     c                   eval      totcom1dsx =   totcom1dx
097500030115     c                   eval      totcos1psx =   totcos1px
097600030115     c                   eval      totcom1psx =   totcom1px
097700030115     c                   eval      totcon1dsx =   totcon1dx
097800030115     c                   eval      totrit1dsx =   totrit1dx
097900030115     c                   eval      totcon1psx =   totcon1px
098000030115     c                   eval      totrit1psx =   totrit1px
098100030115     c                   eval      totcon1dcsx =  totcon1dcx
098200030115     c                   eval      totrit1dcsx =  totrit1dcx
098300030115     c                   eval      totcon1pcsx =  totcon1pcx
098400030115     c                   eval      totrit1pcsx =  totrit1pcx
098500030115
098600030522     c                   eval      totstm1psx =  totstm1px
098700030522     c                   eval      totstm1dsx =  totstm1dx
098800030522     c                   eval      totsco1sx = totstm1dx + totcos1dx
098900030522     c                   eval      totsco1psx = totstm1px + totcos1px
099000030522     c                   eval      totdif1sx = totcom1dx - totsco1sx
099100030522     c                   eval      totdif1psx = totcom1px - totsco1psx
099200030522
099300030203     c                   if        totcom1dx > 0
099400030822     c                   eval      totper18_2= ((totcom1dx - totsco1sx) * 100)
099500030115     c                                        / totcom1dx
099600030822     c                   Z-ADD     totper18_2    totper1sx
099700030822      *
099800030203     c                   setoff                                       7071
099900030203     c     totper1sx     comp      999                                70
100000030203     c  n70totper1sx     comp      -999                                 71
100100030203     c   70              z-add     999           totper1sx
100200030203     c   71              z-add     -999          totper1sx
100300030116     c                   else
100400030116     c                   z-add     0             totper1sx
100500030115     c                   endif
100600030115
100700030203     c                   if        totcom1px > 0
100800030822     c                   eval      totper18_2 = ((totcom1px - totsco1psx) * 100)
100900030115     c                                        / totcom1px
101000030822     c                   Z-ADD     totper18_2    totper1psx
101100030822      *
101200030203     c                   setoff                                       7071
101300030203     c     totper1psx    comp      999                                70
101400030203     c  n70totper1psx    comp      -999                                 71
101500030203     c   70              z-add     999           totper1psx
101600030203     c   71              z-add     -999          totper1psx
101700030116     c                   else
101800030116     c                   z-add     0             totper1psx
101900030115     c                   endif
102000030515      *imposto i  campi x la stampa affluenze defluenze
102100030515     c                   eval      totcos1dsa =   totcos1da
102200030515     c                   eval      totcom1dsa =   totcom1da
102300030515     c                   eval      totcos1psa =   totcos1pa
102400030515     c                   eval      totcom1psa =   totcom1pa
102500030515     c                   eval      totcon1dsa =   totcon1da
102600030515     c                   eval      totrit1dsa =   totrit1da
102700030515     c                   eval      totcon1psa =   totcon1pa
102800030515     c                   eval      totrit1psa =   totrit1pa
102900030515     c                   eval      totcon1dcsa =  totcon1dca
103000030515     c                   eval      totrit1dcsa =  totrit1dca
103100030515     c                   eval      totcon1pcsa =  totcon1pca
103200030515     c                   eval      totrit1pcsa =  totrit1pca
103300030515
103400030522     c                   eval      totstm1psa =  totstm1pa
103500030522     c                   eval      totstm1dsa =  totstm1da
103600030522     c                   eval      totsco1sa = totstm1da + totcos1da
103700030522     c                   eval      totsco1psa = totstm1pa + totcos1pa
103800030522     c                   eval      totdif1sa = totcom1da - totsco1sa
103900030522     c                   eval      totdif1psa = totcom1pa - totsco1psa
104000030522
104100030515     c                   if        totcom1da > 0
104200030822     c                   eval      totper18_2= ((totcom1da - totsco1sa) * 100)
104300030515     c                                        / totcom1da
104400030822     c                   Z-ADD     totper18_2    totper1sa
104500030822      *
104600030515     c                   setoff                                       7071
104700030515     c     totper1sa     comp      999                                70
104800030515     c  n70totper1sa     comp      -999                                 71
104900030515     c   70              z-add     999           totper1sa
105000030515     c   71              z-add     -999          totper1sa
105100030515     c                   else
105200030515     c                   z-add     0             totper1sa
105300030515     c                   endif
105400030515
105500030515     c                   if        totcom1pa > 0
105600030822     c                   eval      totper18_2 = ((totcom1pa - totsco1psa) * 100)
105700030515     c                                        / totcom1pa
105800030822     c                   Z-ADD     totper18_2    totper1psa
105900030822      *
106000030515     c                   setoff                                       7071
106100030515     c     totper1psa    comp      999                                70
106200030515     c  n70totper1psa    comp      -999                                 71
106300030515     c   70              z-add     999           totper1psa
106400030515     c   71              z-add     -999          totper1psa
106500030515     c                   else
106600030515     c                   z-add     0             totper1psa
106700030515     c                   endif
106800030528      * ricavo i totali per punto operativo
106900030528      *giornalieri
107000030528     c                   eval      stime1 = totstm1ds + totstm1dsx + totstm1dsa
107100030528     c                   eval      reali1 = totcos1ds + totcos1dsx + totcos1dsa
107200030528     c                   eval      costi1 = totsco1s + totsco1sx + totsco1sa
107300030528     c                   eval      compe1 = totcom1ds + totcom1dsx + totcom1dsa
107400030528     c                   eval      diffe1 = totdif1s + totdif1sx + totdif1sa
107500030528
107600030528      *periodo
107700030528     c                   eval      stime1p= totstm1ps + totstm1psx + totstm1psa
107800030528     c                   eval      reali1p= totcos1ps + totcos1psx + totcos1psa
107900030528     c                   eval      costi1p= totsco1ps + totsco1psx + totsco1psa
108000030528     c                   eval      compe1p= totcom1ps + totcom1psx + totcom1psa
108100030528     c                   eval      diffe1p= totdif1ps + totdif1psx + totdif1psa
108200030528      *calcola le percentuali
108300030528     c                   if        compe1  > 0
108400030822     c                   eval      totper18_2= ((compe1 -costi1) * 100) /compe1
108500030822     c                   Z-ADD     totper18_2    tpcompe1
108600030822      *
108700030528     c                   setoff                                       7071
108800030528     c     tpcompe1      comp      999                                70
108900030528     c  n70tpcompe1      comp      -999                                 71
109000030528     c   70              z-add     999           tpcompe1
109100030528     c   71              z-add     -999          tpcompe1
109200030528     c                   else
109300030528     c                   z-add     0             tpcompe1
109400030528     c                   endif
109500030528
109600030528     c                   if        compe1p > 0
109700030822     c                   eval      totper18_2 = ((compe1p-costi1p) * 100)
109800030822     c                                         /compe1p
109900030822     c                   Z-ADD     totper18_2    tpcompe1p
110000030822      *
110100030528     c                   setoff                                       7071
110200030528     c     tpcompe1p     comp      999                                70
110300030528     c  n70tpcompe1p     comp      -999                                 71
110400030528     c   70              z-add     999           tpcompe1p
110500030528     c   71              z-add     -999          tpcompe1p
110600030528     c                   else
110700030528     c                   z-add     0             tpcompe1p
110800030528     c                   endif
110900030922      *scrivo file riepiloghi se lavoro schedulato
111000030922     c                   if        schedulato <> *blank
111100030922     c                   exsr      riepiloghi
111200030922     c                   endif
111300030922
111400020625     c                   except    detta
111500030115      *azzero i campi livello filiale autotras.
111600020620     c                   clear                   totcos1d
111700020620     c                   clear                   totcom1d
111800020620     c                   clear                   totcos1p
111900020620     c                   clear                   totcom1p
112000020620     c                   clear                   totcon1d
112100020620     c                   clear                   totrit1d
112200020620     c                   clear                   totcon1p
112300020620     c                   clear                   totrit1p
112400020620     c                   clear                   totcon1dc
112500020620     c                   clear                   totrit1dc
112600020620     c                   clear                   totcon1pc
112700020620     c                   clear                   totrit1pc
112800030522
112900030522     c                   clear                   totstm1p
113000030522     c                   clear                   totstm1d
113100030115      *coop
113200030115     c                   clear                   totcos1dx
113300030115     c                   clear                   totcom1dx
113400030115     c                   clear                   totcos1px
113500030115     c                   clear                   totcom1px
113600030115     c                   clear                   totcon1dx
113700030115     c                   clear                   totrit1dx
113800030115     c                   clear                   totcon1px
113900030115     c                   clear                   totrit1px
114000030115     c                   clear                   totcon1dcx
114100030115     c                   clear                   totrit1dcx
114200030115     c                   clear                   totcon1pcx
114300030115     c                   clear                   totrit1pcx
114400030522
114500030522     c                   clear                   totstm1px
114600030522     c                   clear                   totstm1dx
114700030515      *aff/def
114800030515     c                   clear                   totcos1da
114900030515     c                   clear                   totcom1da
115000030515     c                   clear                   totcos1pa
115100030515     c                   clear                   totcom1pa
115200030515     c                   clear                   totcon1da
115300030515     c                   clear                   totrit1da
115400030515     c                   clear                   totcon1pa
115500030515     c                   clear                   totrit1pa
115600030515     c                   clear                   totcon1dca
115700030515     c                   clear                   totrit1dca
115800030515     c                   clear                   totcon1pca
115900030515     c                   clear                   totrit1pca
116000030522
116100030522     c                   clear                   totstm1pa
116200030522     c                   clear                   totstm1da
116300020620     c                   z-add     1             livello
116400020620     c                   end
116500020620      *rottura area
116600020620     c                   if        orgcar <> exarea
116700020625     c                   eval      totcos2ds =   totcos2d
116800020625     c                   eval      totcom2ds =   totcom2d
116900020625     c                   eval      totcos2ps =   totcos2p
117000020625     c                   eval      totcom2ps =   totcom2p
117100020625     c                   eval      totcon2ds =   totcon2d
117200020625     c                   eval      totrit2ds =   totrit2d
117300020625     c                   eval      totcon2ps =   totcon2p
117400020625     c                   eval      totrit2ps =   totrit2p
117500020625     c                   eval      totcon2dcs =   totcon2dc
117600020625     c                   eval      totrit2dcs =   totrit2dc
117700020625     c                   eval      totcon2pcs =   totcon2pc
117800020625     c                   eval      totrit2pcs =   totrit2pc
117900020625
118000030522     c                   eval      totstm2ps  =  totstm2p
118100030522     c                   eval      totstm2ds  =  totstm2d
118200030522     c                   eval      totsco2s  = totstm2d  + totcos2d
118300030522     c                   eval      totsco2ps  = totstm2p  + totcos2p
118400030522     c                   eval      totdif2s  = totcom2d  - totsco2s
118500030522     c                   eval      totdif2ps  = totcom2p  - totsco2ps
118600030522
118700030203     c                   if        totcom2d > 0
118800030822     c                   eval      totper18_2 = ((totcom2d - totsco2s) * 100)
118900020625     c                                        / totcom2d
119000030822     c                   Z-ADD     totper18_2    totper2s
119100030822      *
119200030203     c                   setoff                                       7071
119300030203     c     totper2s      comp      999                                70
119400030203     c  n70totper2s      comp      -999                                 71
119500030203     c   70              z-add     999           totper2s
119600030203     c   71              z-add     -999          totper2s
119700030116     c                   else
119800030116     c                   z-add     0             totper2s
119900020625     c                   endif
120000020625
120100030203     c                   if        totcom2p > 0
120200030822     c                   eval      totper18_2= ((totcom2p - totsco2ps) * 100)
120300020625     c                                        / totcom2p
120400030822     c                   Z-ADD     totper18_2    totper2ps
120500030822      *
120600030203     c                   setoff                                       7071
120700030203     c     totper2ps     comp      999                                70
120800030203     c  n70totper2ps     comp      -999                                 71
120900030203     c   70              z-add     999           totper2ps
121000030203     c   71              z-add     -999          totper2ps
121100030116     c                   else
121200030116     c                   z-add     0             totper2ps
121300020625     c                   endif
121400030115      *coop
121500030115     c                   eval      totcos2dsx =   totcos2dx
121600030115     c                   eval      totcom2dsx =   totcom2dx
121700030115     c                   eval      totcos2psx =   totcos2px
121800030115     c                   eval      totcom2psx =   totcom2px
121900030115     c                   eval      totcon2dsx =   totcon2dx
122000030115     c                   eval      totrit2dsx =   totrit2dx
122100030115     c                   eval      totcon2psx =   totcon2px
122200030115     c                   eval      totrit2psx =   totrit2px
122300030115     c                   eval      totcon2dcsx =   totcon2dcx
122400030115     c                   eval      totrit2dcsx =   totrit2dcx
122500030115     c                   eval      totcon2pcsx =   totcon2pcx
122600030115     c                   eval      totrit2pcsx =   totrit2pcx
122700030115
122800030522     c                   eval      totstm2psx =  totstm2px
122900030522     c                   eval      totstm2dsx =  totstm2dx
123000030522     c                   eval      totsco2sx = totstm2dx + totcos2dx
123100030522     c                   eval      totsco2psx = totstm2px + totcos2px
123200030522     c                   eval      totdif2sx = totcom2dx - totsco2sx
123300030522     c                   eval      totdif2psx = totcom2px - totsco2psx
123400030522
123500030203     c                   if        totcom2dx > 0
123600030822     c                   eval      totper18_2= ((totcom2dx - totsco2sx) * 100)
123700030115     c                                        / totcom2dx
123800030822     c                   Z-ADD     totper18_2    totper2sx
123900030822      *
124000030203     c                   setoff                                       7071
124100030203     c     totper2sx     comp      999                                70
124200030203     c  n70totper2sx     comp      -999                                 71
124300030203     c   70              z-add     999           totper2sx
124400030203     c   71              z-add     -999          totper2sx
124500030116     c                   else
124600030116     c                   z-add     0             totper2sx
124700030115     c                   endif
124800030115
124900030203     c                   if        totcom2px> 0
125000030822     c                   eval      totper18_2 = ((totcom2px - totsco2psx) * 100)
125100030115     c                                        / totcom2px
125200030822     c                   Z-ADD     totper18_2    totper2psx
125300030822      *
125400030203     c                   setoff                                       7071
125500030203     c     totper2psx    comp      999                                70
125600030203     c  n70totper2psx    comp      -999                                 71
125700030203     c   70              z-add     999           totper2psx
125800030203     c   71              z-add     -999          totper2psx
125900030116     c                   else
126000030116     c                   z-add     0             totper2psx
126100030115     c                   endif
126200030515      *aff/def
126300030515     c                   eval      totcos2dsa =   totcos2da
126400030515     c                   eval      totcom2dsa =   totcom2da
126500030515     c                   eval      totcos2psa =   totcos2pa
126600030515     c                   eval      totcom2psa =   totcom2pa
126700030515     c                   eval      totcon2dsa =   totcon2da
126800030515     c                   eval      totrit2dsa =   totrit2da
126900030515     c                   eval      totcon2psa =   totcon2pa
127000030515     c                   eval      totrit2psa =   totrit2pa
127100030515     c                   eval      totcon2dcsa =   totcon2dca
127200030515     c                   eval      totrit2dcsa =   totrit2dca
127300030515     c                   eval      totcon2pcsa =   totcon2pca
127400030515     c                   eval      totrit2pcsa =   totrit2pca
127500030515
127600030522     c                   eval      totstm2psa =  totstm2pa
127700030522     c                   eval      totstm2dsa =  totstm2da
127800030522     c                   eval      totsco2sa = totstm2da + totcos2da
127900030522     c                   eval      totsco2psa = totstm2pa + totcos2pa
128000030522     c                   eval      totdif2sa = totcom2da - totsco2sa
128100030522     c                   eval      totdif2psa = totcom2pa - totsco2psa
128200030522
128300030515     c                   if        totcom2da > 0
128400030822     c                   eval      totper18_2= ((totcom2da - totsco2sa) * 100)
128500030515     c                                        / totcom2da
128600030822     c                   Z-ADD     totper18_2    totper2sa
128700030822      *
128800030515     c                   setoff                                       7071
128900030515     c     totper2sa     comp      999                                70
129000030515     c  n70totper2sa     comp      -999                                 71
129100030515     c   70              z-add     999           totper2sa
129200030515     c   71              z-add     -999          totper2sa
129300030515     c                   else
129400030515     c                   z-add     0             totper2sa
129500030515     c                   endif
129600030515
129700030515     c                   if        totcom2pa> 0
129800030822     c                   eval      totper18_2 = ((totcom2pa - totsco2psa) * 100)
129900030515     c                                        / totcom2pa
130000030822     c                   Z-ADD     totper18_2    totper2psa
130100030822      *
130200030515     c                   setoff                                       7071
130300030515     c     totper2psa    comp      999                                70
130400030515     c  n70totper2psa    comp      -999                                 71
130500030515     c   70              z-add     999           totper2psa
130600030515     c   71              z-add     -999          totper2psa
130700030515     c                   else
130800030515     c                   z-add     0             totper2psa
130900030515     c                   endif
131000030528      * ricavo i totali per area
131100030528      *giornalieri
131200030528     c                   eval      stime2 = totstm2ds + totstm2dsx + totstm2dsa
131300030528     c                   eval      reali2 = totcos2ds + totcos2dsx + totcos2dsa
131400030528     c                   eval      costi2 = totsco2s + totsco2sx + totsco2sa
131500030528     c                   eval      compe2 = totcom2ds + totcom2dsx + totcom2dsa
131600030528     c                   eval      diffe2 = totdif2s + totdif2sx + totdif2sa
131700030528
131800030528      *periodo
131900030528     c                   eval      stime2p= totstm2ps + totstm2psx + totstm2psa
132000030528     c                   eval      reali2p= totcos2ps + totcos2psx + totcos2psa
132100030528     c                   eval      costi2p= totsco2ps + totsco2psx + totsco2psa
132200030528     c                   eval      compe2p= totcom2ps + totcom2psx + totcom2psa
132300030528     c                   eval      diffe2p= totdif2ps + totdif2psx + totdif2psa
132400030528      *calcola le percentuali
132500030528     c                   if        compe2  > 0
132600030822     c                   eval      totper18_2= ((compe2 -costi2) * 100) /compe2
132700030822     c                   Z-ADD     totper18_2    tpcompe2
132800030822      *
132900030528     c                   setoff                                       7071
133000030528     c     tpcompe2      comp      999                                70
133100030528     c  n70tpcompe2      comp      -999                                 71
133200030528     c   70              z-add     999           tpcompe2
133300030528     c   71              z-add     -999          tpcompe2
133400030528     c                   else
133500030528     c                   z-add     0             tpcompe2
133600030528     c                   endif
133700030528
133800030528     c                   if        compe2p > 0
133900030822     c                   eval      totper18_2=((compe2p-costi2p) * 100) /compe2p
134000030822     c                   Z-ADD     totper18_2    tpcompe2p
134100030822      *
134200030528     c                   setoff                                       7071
134300030528     c     tpcompe2p     comp      999                                70
134400030528     c  n70tpcompe2p     comp      -999                                 71
134500030528     c   70              z-add     999           tpcompe2p
134600030528     c   71              z-add     -999          tpcompe2p
134700030528     c                   else
134800030528     c                   z-add     0             tpcompe2p
134900030528     c                   endif
135000030528
135100020620     c                   except    area
135200020620     c                   z-add     66            lin2
135300020620      *azzero i campi livello area
135400020620     c                   clear                   totcos2d
135500020620     c                   clear                   totcom2d
135600020620     c                   clear                   totcos2p
135700020620     c                   clear                   totcom2p
135800020620     c                   clear                   totcon2d
135900020620     c                   clear                   totrit2d
136000020620     c                   clear                   totcon2p
136100020620     c                   clear                   totrit2p
136200020620     c                   clear                   totcon2dc
136300020620     c                   clear                   totrit2dc
136400020620     c                   clear                   totcon2pc
136500020620     c                   clear                   totrit2pc
136600030522
136700030522     c                   clear                   totstm2p
136800030522     c                   clear                   totstm2d
136900030115      *coop
137000030115     c                   clear                   totcos2dx
137100030115     c                   clear                   totcom2dx
137200030115     c                   clear                   totcos2px
137300030115     c                   clear                   totcom2px
137400030115     c                   clear                   totcon2dx
137500030115     c                   clear                   totrit2dx
137600030115     c                   clear                   totcon2px
137700030115     c                   clear                   totrit2px
137800030115     c                   clear                   totcon2dcx
137900030115     c                   clear                   totrit2dcx
138000030115     c                   clear                   totcon2pcx
138100030115     c                   clear                   totrit2pcx
138200030522
138300030522     c                   clear                   totstm2px
138400030522     c                   clear                   totstm2dx
138500030515      *aff/def
138600030515     c                   clear                   totcos2da
138700030515     c                   clear                   totcom2da
138800030515     c                   clear                   totcos2pa
138900030515     c                   clear                   totcom2pa
139000030515     c                   clear                   totcon2da
139100030515     c                   clear                   totrit2da
139200030515     c                   clear                   totcon2pa
139300030515     c                   clear                   totrit2pa
139400030515     c                   clear                   totcon2dca
139500030515     c                   clear                   totrit2dca
139600030515     c                   clear                   totcon2pca
139700030515     c                   clear                   totrit2pca
139800030522
139900030522     c                   clear                   totstm2pa
140000030522     c                   clear                   totstm2da
140100020620     c                   z-add     2             livello
140200020620     c                   end
140300020620
140400020620      *rottura distretto
140500020620     c                   if        orgfl3 <> exdistr
140600020625     c                   eval      totcos3ds =   totcos3d
140700020625     c                   eval      totcom3ds =   totcom3d
140800020625     c                   eval      totcos3ps =   totcos3p
140900020625     c                   eval      totcom3ps =   totcom3p
141000020625     c                   eval      totcon3ds =   totcon3d
141100020625     c                   eval      totrit3ds =   totrit3d
141200020625     c                   eval      totcon3ps =   totcon3p
141300020625     c                   eval      totrit3ps =   totrit3p
141400020625     c                   eval      totcon3dcs =  totcon3dc
141500020625     c                   eval      totrit3dcs =  totrit3dc
141600020625     c                   eval      totcon3pcs =  totcon3pc
141700020625     c                   eval      totrit3pcs =  totrit3pc
141800030522
141900030522     c                   eval      totstm3ps  =  totstm3p
142000030522     c                   eval      totstm3ds  =  totstm3d
142100030522     c                   eval      totsco3s  = totstm3d  + totcos3d
142200030522     c                   eval      totsco3ps  = totstm3p  + totcos3p
142300030522     c                   eval      totdif3s  = totcom3d  - totsco3s
142400030522     c                   eval      totdif3ps  = totcom3p  - totsco3ps
142500030522
142600030203     c                   if        totcom3d > 0
142700030822     c                   eval      totper18_2 = ((totcom3d - totsco3s) * 100)
142800020625     c                                        / totcom3d
142900030822     c                   Z-ADD     totper18_2    totper3s
143000030822      *
143100030203     c                   setoff                                       7071
143200030203     c     totper3s      comp      999                                70
143300030203     c  n70totper3s      comp      -999                                 71
143400030203     c   70              z-add     999           totper3s
143500030203     c   71              z-add     -999          totper3s
143600030116     c                   else
143700030116     c                   z-add     0             totper3s
143800020625     c                   endif
143900030203     c                   if        totcom3p > 0
144000030822     c                   eval      totper18_2= ((totcom3p - totsco3ps) * 100)
144100020625     c                                        / totcom3p
144200030822     c                   Z-ADD     totper18_2    totper3ps
144300030822      *
144400030203     c                   setoff                                       7071
144500030203     c     totper3ps     comp      999                                70
144600030203     c  n70totper3ps     comp      -999                                 71
144700030203     c   70              z-add     999           totper3ps
144800030203     c   71              z-add     -999          totper3ps
144900030116     c                   else
145000030116     c                   z-add     0             totper3ps
145100020625     c                   endif
145200030116      *coop
145300030116     c                   eval      totcos3dsx =   totcos3dx
145400030116     c                   eval      totcom3dsx =   totcom3dx
145500030116     c                   eval      totcos3psx =   totcos3px
145600030116     c                   eval      totcom3psx =   totcom3px
145700030116     c                   eval      totcon3dsx =   totcon3dx
145800030116     c                   eval      totrit3dsx =   totrit3dx
145900030116     c                   eval      totcon3psx =   totcon3px
146000030116     c                   eval      totrit3psx =   totrit3px
146100030116     c                   eval      totcon3dcsx =  totcon3dcx
146200030116     c                   eval      totrit3dcsx =  totrit3dcx
146300030116     c                   eval      totcon3pcsx =  totcon3pcx
146400030116     c                   eval      totrit3pcsx =  totrit3pcx
146500030522
146600030522     c                   eval      totstm3psx =  totstm3px
146700030522     c                   eval      totstm3dsx =  totstm3dx
146800030522     c                   eval      totsco3sx = totstm3dx + totcos3dx
146900030522     c                   eval      totsco3psx = totstm3px + totcos3px
147000030522     c                   eval      totdif3sx = totcom3dx - totsco3sx
147100030522     c                   eval      totdif3psx = totcom3px - totsco3psx
147200030203
147300030203     c                   if        totcom3dx > 0
147400030822     c                   eval      totper18_2= ((totcom3dx - totsco3sx) * 100)
147500030116     c                                        / totcom3dx
147600030822     c                   Z-ADD     totper18_2    totper3sx
147700030822      *
147800030203     c                   setoff                                       7071
147900030203     c     totper3sx     comp      999                                70
148000030203     c  n70totper3sx     comp      -999                                 71
148100030203     c   70              z-add     999           totper3sx
148200030203     c   71              z-add     -999          totper3sx
148300030116     c                   else
148400030116     c                   z-add     0             totper3sx
148500030116     c                   endif
148600030203     c                   if        totcom3px > 0
148700030822     c                   eval      totper18_2 = ((totcom3px - totsco3psx) * 100)
148800030116     c                                        / totcom3px
148900030822     c                   Z-ADD     totper18_2    totper3psx
149000030822      *
149100030203     c                   setoff                                       7071
149200030203     c     totper3psx    comp      999                                70
149300030203     c  n70totper3psx    comp      -999                                 71
149400030203     c   70              z-add     999           totper3psx
149500030203     c   71              z-add     -999          totper3psx
149600030116     c                   else
149700030116     c                   z-add     0             totper3psx
149800030116     c                   endif
149900030515      *aff/def
150000030515     c                   eval      totcos3dsa =   totcos3da
150100030515     c                   eval      totcom3dsa =   totcom3da
150200030515     c                   eval      totcos3psa =   totcos3pa
150300030515     c                   eval      totcom3psa =   totcom3pa
150400030515     c                   eval      totcon3dsa =   totcon3da
150500030515     c                   eval      totrit3dsa =   totrit3da
150600030515     c                   eval      totcon3psa =   totcon3pa
150700030515     c                   eval      totrit3psa =   totrit3pa
150800030515     c                   eval      totcon3dcsa =  totcon3dca
150900030515     c                   eval      totrit3dcsa =  totrit3dca
151000030515     c                   eval      totcon3pcsa =  totcon3pca
151100030515     c                   eval      totrit3pcsa =  totrit3pca
151200030515
151300030522     c                   eval      totstm3psa =  totstm3pa
151400030522     c                   eval      totstm3dsa =  totstm3da
151500030522     c                   eval      totsco3sa = totstm3da + totcos3da
151600030522     c                   eval      totsco3psa = totstm3pa + totcos3pa
151700030522     c                   eval      totdif3sa = totcom3da - totsco3sa
151800030522     c                   eval      totdif3psa = totcom3pa - totsco3psa
151900030522
152000030515     c                   if        totcom3da > 0
152100030822     c                   eval      totper18_2= ((totcom3da - totsco3sa) * 100)
152200030515     c                                        / totcom3da
152300030822     c                   Z-ADD     totper18_2    totper3sa
152400030822      *
152500030515     c                   setoff                                       7071
152600030515     c     totper3sa     comp      999                                70
152700030515     c  n70totper3sa     comp      -999                                 71
152800030515     c   70              z-add     999           totper3sa
152900030515     c   71              z-add     -999          totper3sa
153000030515     c                   else
153100030515     c                   z-add     0             totper3sa
153200030515     c                   endif
153300030515     c                   if        totcom3pa > 0
153400030822     c                   eval      totper18_2 = ((totcom3pa - totsco3psa) * 100)
153500030515     c                                        / totcom3pa
153600030822     c                   Z-ADD     totper18_2    totper3psa
153700030822      *
153800030515     c                   setoff                                       7071
153900030515     c     totper3psa    comp      999                                70
154000030515     c  n70totper3psa    comp      -999                                 71
154100030515     c   70              z-add     999           totper3psa
154200030515     c   71              z-add     -999          totper3psa
154300030515     c                   else
154400030515     c                   z-add     0             totper3psa
154500030515     c                   endif
154600030528      * ricavo i totali per distretto
154700030528      *giornalieri
154800030528     c                   eval      stime3 = totstm3ds + totstm3dsx + totstm3dsa
154900030528     c                   eval      reali3 = totcos3ds + totcos3dsx + totcos3dsa
155000030528     c                   eval      costi3 = totsco3s + totsco3sx + totsco3sa
155100030528     c                   eval      compe3 = totcom3ds + totcom3dsx + totcom3dsa
155200030528     c                   eval      diffe3 = totdif3s + totdif3sx + totdif3sa
155300030528
155400030528      *periodo
155500030528     c                   eval      stime3p= totstm3ps + totstm3psx + totstm3psa
155600030528     c                   eval      reali3p= totcos3ps + totcos3psx + totcos3psa
155700030528     c                   eval      costi3p= totsco3ps + totsco3psx + totsco3psa
155800030528     c                   eval      compe3p= totcom3ps + totcom3psx + totcom3psa
155900030528     c                   eval      diffe3p= totdif3ps + totdif3psx + totdif3psa
156000030528      *calcola le percentuali
156100030528     c                   if        compe3  > 0
156200030822     c                   eval      totper18_2= ((compe3 -costi3) * 100) /compe3
156300030822     c                   Z-ADD     totper18_2    tpcompe3
156400030822      *
156500030528     c                   setoff                                       7071
156600030528     c     tpcompe3      comp      999                                70
156700030528     c  n70tpcompe3      comp      -999                                 71
156800030528     c   70              z-add     999           tpcompe3
156900030528     c   71              z-add     -999          tpcompe3
157000030528     c                   else
157100030528     c                   z-add     0             tpcompe3
157200030528     c                   endif
157300030528
157400030528     c                   if        compe3p > 0
157500030822     c                   eval      totper18_2=((compe3p-costi3p) * 100) /compe3p
157600030822     c                   Z-ADD     totper18_2    tpcompe3p
157700030822      *
157800030528     c                   setoff                                       7071
157900030528     c     tpcompe3p     comp      999                                70
158000030528     c  n70tpcompe3p     comp      -999                                 71
158100030528     c   70              z-add     999           tpcompe3p
158200030528     c   71              z-add     -999          tpcompe3p
158300030528     c                   else
158400030528     c                   z-add     0             tpcompe3p
158500030528     c                   endif
158600030528
158700020620     c                   except    distret
158800020620     c                   z-add     66            lin3
158900030114      *azzero i campi livello distretto
159000030116     c                   clear                   totcos3d
159100030116     c                   clear                   totcom3d
159200030116     c                   clear                   totcos3p
159300030116     c                   clear                   totcom3p
159400030116     c                   clear                   totcon3d
159500030116     c                   clear                   totrit3d
159600030116     c                   clear                   totcon3p
159700030116     c                   clear                   totrit3p
159800030116     c                   clear                   totcon3dc
159900030116     c                   clear                   totrit3dc
160000020620     c                   clear                   totcon3pc
160100030116     c                   clear                   totrit3pc
160200030522
160300030522     c                   clear                   totstm3p
160400030522     c                   clear                   totstm3d
160500030116      *coop
160600030116     c                   clear                   totcos3dx
160700030116     c                   clear                   totcom3dx
160800030116     c                   clear                   totcos3px
160900030116     c                   clear                   totcom3px
161000030116     c                   clear                   totcon3dx
161100030116     c                   clear                   totrit3dx
161200030116     c                   clear                   totcon3px
161300030116     c                   clear                   totrit3px
161400030116     c                   clear                   totcon3dcx
161500030116     c                   clear                   totrit3dcx
161600030116     c                   clear                   totcon3pcx
161700030116     c                   clear                   totrit3pcx
161800030522
161900030522     c                   clear                   totstm3px
162000030522     c                   clear                   totstm3dx
162100030515      *aff/def
162200030515     c                   clear                   totcos3da
162300030515     c                   clear                   totcom3da
162400030515     c                   clear                   totcos3pa
162500030515     c                   clear                   totcom3pa
162600030515     c                   clear                   totcon3da
162700030515     c                   clear                   totrit3da
162800030515     c                   clear                   totcon3pa
162900030515     c                   clear                   totrit3pa
163000030515     c                   clear                   totcon3dca
163100030515     c                   clear                   totrit3dca
163200030515     c                   clear                   totcon3pca
163300030515     c                   clear                   totrit3pca
163400030522
163500030522     c                   clear                   totstm3pa
163600030522     c                   clear                   totstm3da
163700030116     c                   z-add     3             livello
163800020620     c                   end
163900020620
164000020620     c                   exsr      invia
164100100210     c                   end
164200020620     c                   endsr
164300020521      *------------------------------------------------------
164400020521     c     elabora       begsr
164500020521      *------------------------------------------------------
164600020611     c                   clear                   rekok
164700030606     c                   clear                   exfssddc
164800030115      *elabora prima i totali coop per la filiale
164900030115     c                   exsr      elaboracoop
165000030115
165100030114      *loop record saldi autotrasportatori
165200020612     c     kfss          setll     fifss01l
165300020517     c                   do        *hival
165400020612     c                   read      fifss01l                               98
165500020517     c   98              leave
165600030114      *rottura data o filiale
165700020611     c                   if        fssddc > alladat or fssfgs <> orgfil
165800020611     c                   leave
165900020611     c                   endif
166000020612
166100030115      *imposta campi per le rotture
166200020612     c                   if        rekok = *blank
166300020612     c                   select
166400020612     c                   when      livello = 3
166500020612     c                   move      orgfl3        exdistr
166600020612     c                   move      orgcar        exarea
166700020612     c                   move      orgfil        exfil
166800020612     c                   when      livello = 2
166900020612     c                   move      orgcar        exarea
167000020612     c                   move      orgfil        exfil
167100020612     c                   when      livello = 1
167200020612     c                   move      orgfil        exfil
167300020612     c                   endsl
167400020619
167500020619     c                   movel     orgdes        desfil
167600020619
167700020612     c                   exsr      imbuca
167800020612     c                   endif
167900020611
168000020611     c                   move      'X'           rekok
168100030516      *totalizza stime autotrasportatori per giornata
168200030527      *in questo caso controllo la data perchè per la stessa ho più rek
168300030527      *con codici aut. diversi le stime devo prenderle una sola volta x data
168400151014      *verifica se filiale partita con nuove stime da tabella VPO
168500151014     c                   if        datavpo = 0 or dataVpo > fsSddc
168600030527     c                   if        fssddc <> exfssddc
168700030527     c                   move      fssddc        exfssddc
168800030515     c                   move      *zeros        kfrepdr
168900030515     c                   movel     orgfil        kfrepdr
169000030527     c                   move      fssddc        kfreddc
169100030515     c                   move      'A'           kfretsr
169200030526     c                   move      'G'           kfretip
169300030515     c                   clear                   importo
169400030515     c                   exsr      stime
169500030527     c                   else
169600030527     c                   clear                   importo
169700030527     c                   end
169800151014     C                   ELSE
169900151014     c*
170000151016     c                   if        fssddc <> exfssddc
170100151016     c                   move      fssddc        exfssddc
170200151014     c                   movea     pAave         pvocA
170300151014     c                   movea     pAdar         pvocD
170400151014     c                   move      fsSddc        pdata
170500151014     c                   move      orgfil        pfil
170600151014     c                   move      'A'           ptsr
170700151014     c                   clear                   pimpo
170800151014     c                   call      'FICN34SR'
170900151014     c                   parm                    FICN34S
171000151014     c                   z-add     pimpo         importo
171100151016     c                   else
171200151016     c                   clear                   importo
171300151016     c                   end
171400151014     c*
171500151014     c                   endif
171600080924
171700080924     c* somma nei costi stimati eventuali aumenti costo carburante
171800080924     c                   add       fssxx1        importo
171900020612
172000020614      *totalizza i costi del record
172100020620     c                   eval      cosparz  = (fss001 + fss005
172200020614     c                             + fss016 + fss019 + fss034 + fss035
172300020620     c                             + fss997 + fss998)
172400020614
172500020614      *totalizza le competenze del record
172600020620     c                   eval      comparz  = (fss001b + fss005b
172700020614     c                             + fss016b + fss019b + fss034b + fss035b
172800030430     c                             + fss997b + fss998b + fssxx1b + fssxx2b)
172900020614
173000020619      *totalizza i costi solo consegne
173100020620     c                   eval      conparz  = (fss005 + fss016
173200020620     c                             + fss019 + fss034 + fss998)
173300020619
173400020619      *totalizza i costi solo ritiri
173500020620     c                   eval      ritparz = (fss001 +fss035 + fss997)
173600020619
173700020619      *totalizza le competenze solo consegne
173800020620     c                   eval      conparzc = (fss005b +  + fss016b
173900030430     c                             + fss019b + fss034b + fss998b
174000030430     c                             + fssxx1b + fssxx2b)
174100020619
174200020619      *totalizza le competenze solo ritiri
174300020620     c                   eval      ritparzc = (fss001b + fss035b)
174400020619
174500020619      *controllo  per sapere dove totalizzare se nel periodo o alla data
174600030225     c                   if        fssddc >= alladatx
174700030515      *stime giorno in esame
174800030515     c                   add       importo       totstm1d
174900030515     c                   add       importo       totstm2d
175000030515     c                   add       importo       totstm3d
175100030515     c                   add       importo       totstm4d
175200020614      *costi giorno in esame
175300020614     c                   add       cosparz       totcos1d
175400020614     c                   add       cosparz       totcos2d
175500020614     c                   add       cosparz       totcos3d
175600020614     c                   add       cosparz       totcos4d
175700020614      *competenze giorno in esame
175800020614     c                   add       comparz       totcom1d
175900020614     c                   add       comparz       totcom2d
176000020614     c                   add       comparz       totcom3d
176100020614     c                   add       comparz       totcom4d
176200020619      *costi giorno in esame solo consegne
176300020619     c                   add       conparz       totcon1d
176400020619     c                   add       conparz       totcon2d
176500020619     c                   add       conparz       totcon3d
176600020619     c                   add       conparz       totcon4d
176700020619      *costi giorno in esame solo ritiri
176800020619     c                   add       ritparz       totrit1d
176900020619     c                   add       ritparz       totrit2d
177000020619     c                   add       ritparz       totrit3d
177100020619     c                   add       ritparz       totrit4d
177200020619      *competenze giorno in esame solo consegne
177300020619     c                   add       conparzc      totcon1dc
177400020619     c                   add       conparzc      totcon2dc
177500020619     c                   add       conparzc      totcon3dc
177600020619     c                   add       conparzc      totcon4dc
177700020619      *competenze giorno in esame solo ritiri
177800020619     c                   add       ritparzc      totrit1dc
177900020619     c                   add       ritparzc      totrit2dc
178000020619     c                   add       ritparzc      totrit3dc
178100020619     c                   add       ritparzc      totrit4dc
178200030225     c                   endif
178300020619      * totalizzazione nel periodo
178400030225     c*                  else
178500030515      *stime periodo  in esame
178600030515     c                   add       importo       totstm1p
178700030515     c                   add       importo       totstm2p
178800030515     c                   add       importo       totstm3p
178900030515     c                   add       importo       totstm4p
179000020614      *costi progressivo periodo
179100020614     c                   add       cosparz       totcos1p
179200020614     c                   add       cosparz       totcos2p
179300020614     c                   add       cosparz       totcos3p
179400020614     c                   add       cosparz       totcos4p
179500020614      *competenze progressivo periodo
179600020619     c                   add       comparz       totcom1p
179700020619     c                   add       comparz       totcom2p
179800020619     c                   add       comparz       totcom3p
179900020619     c                   add       comparz       totcom4p
180000020619      *costi periodo in esame solo consegne
180100020619     c                   add       conparz       totcon1p
180200020619     c                   add       conparz       totcon2p
180300020619     c                   add       conparz       totcon3p
180400020619     c                   add       conparz       totcon4p
180500020619      *costi periodo in esame solo ritiri
180600020619     c                   add       ritparz       totrit1p
180700020619     c                   add       ritparz       totrit2p
180800020619     c                   add       ritparz       totrit3p
180900020619     c                   add       ritparz       totrit4p
181000020619      *competenze periodo in esame solo consegne
181100020620     c                   add       conparzc      totcon1pc
181200020620     c                   add       conparzc      totcon2pc
181300020620     c                   add       conparzc      totcon3pc
181400020620     c                   add       conparzc      totcon4pc
181500020619      *competenze periodo in esame solo ritiri
181600020620     c                   add       ritparzc      totrit1pc
181700020620     c                   add       ritparzc      totrit2pc
181800020620     c                   add       ritparzc      totrit3pc
181900020620     c                   add       ritparzc      totrit4pc
182000030225     c*                  end
182100020611     c
182200020517     c                   enddo
182300100210     c                   if        rekok = *blank and orgfl3 < '0'
182400100210     c                   select
182500100210     c                   when      livello = 3
182600100210     c                   move      orgfl3        exdistr
182700100210     c                   move      orgcar        exarea
182800100210     c                   move      orgfil        exfil
182900100210     c                   when      livello = 2
183000100210     c                   move      orgcar        exarea
183100100210     c                   move      orgfil        exfil
183200100210     c                   when      livello = 1
183300100210     c                   move      orgfil        exfil
183400100210     c                   endsl
183500100210
183600100210     c                   movel     'F'           rekok
183700100210     c                   movel     orgdes        desfil
183800100210     c                   exsr      imbuca
183900100210
184000100210     c                   endif
184100020521     c                   endsr
184200030115      *------------------------------------------------------
184300030115     c     elaboracoop   begsr
184400030115      *------------------------------------------------------
184500030526     c                   exsr      elaboraAFDE
184600030115
184700030115      *loop record saldi coop
184800030115     c     kfss          setll     fifsc01l
184900030115     c                   do        *hival
185000030115     c                   read      fifsc01l                               98
185100030115     c   98              leave
185200030115      *rottura data o filiale
185300030115     c                   if        fscddc > alladat or fscfgs <> orgfil
185400030115     c                   leave
185500030116     c                   endif
185600030516      *totalizza stime coop  per giornata
185700151014      *verifica se filiale partita con nuove stime da tabella VPO
185800151014     c                   if        datavpo = 0 or dataVpo > fscddc
185900061004     c                   move      *zeros        kfrepdr
186000061004     c                   movel     orgfil        kfrepdr
186100061004     c*mm                move      fscpdr        kfrepdr
186200030527     c                   move      fscddc        kfreddc
186300030515     c                   move      'F'           kfretsr
186400030526     c                   move      'C'           kfretip
186500030515     c                   clear                   importo
186600030515     c                   exsr      stime
186700151014     c                   else
186800151014     c*
186900151014     c                   movea     pCave         pvocA
187000151014     c                   movea     pCdar         pvocD
187100151014     c                   move      fscddc        pdata
187200151014     c                   move      orgfil        pfil
187300151014     c                   move      'C'           ptsr
187400151014     c                   clear                   pimpo
187500151014     c                   call      'FICN34SR'
187600151014     c                   parm                    FICN34S
187700151014     c                   z-add     pimpo         importo
187800151014     c*
187900151014     c                   endif
188000030115
188100030115      *totalizza i costi del record
188200030115     c                   eval      cosparzx = (fsc002 + fsc005
188300030115     c                             + fsc006 + fsc009 + fsc010 + fsc011
188400030115     c                             + fsc012 + fsc023 + fsc039 + fsc040
188500030115     c                             + fsc041 + fsc060 + fsc061 + fscxx1
188600030115     c                             + fscxx2)
188700030115
188800030115      *totalizza le competenze del record
188900030115     c                   eval      comparzx = (fsc002b + fsc005b
189000030115     c                             + fsc006b + fsc009b + fsc010b + fsc011b
189100030115     c                             + fsc012b + fsc023b + fsc039b + fsc040b
189200030115     c                             + fsc041b + fsc060b + fsc061b + fscxx1b
189300030115     c                             + fscxx2b)
189400030115
189500030115      *totalizza i costi solo consegne
189600030115     c                   eval      conparzx = (fsc005 + fsc006 + fsc041
189700030115     c                             + fsc010 + fsc011 + fsc012
189800030115     c                             + fsc023 + fsc060 + fsc061)
189900030115
190000030115      *totalizza i costi solo ritiri
190100030115     c                   eval      ritparzx = (fsc002 +fsc009
190200030115     c                             + fsc039 + fsc040)
190300030115
190400030115      *totalizza le competenze solo consegne
190500030115     c                   eval      conparzcx = (fsc005b+ fsc006b+ fsc041b
190600030115     c                             + fsc010b + fsc011b + fsc012b
190700030115     c                             + fsc023b + fsc060b + fsc061b)
190800030115
190900030115      *totalizza le competenze solo ritiri
191000030115     c                   eval      ritparzcx = (fsc002b + fsc009b
191100030115     c                             + fsc039b + fsc040b)
191200030115
191300030115      *controllo  per sapere dove totalizzare se nel periodo o alla data
191400030225     c                   if        fscddc >= alladatx
191500030515      *stime giorno in esame
191600030515     c                   add       importo       totstm1dx
191700030515     c                   add       importo       totstm2dx
191800030515     c                   add       importo       totstm3dx
191900030515     c                   add       importo       totstm4dx
192000030115      *costi giorno in esame
192100030115     c                   add       cosparzx      totcos1dx
192200030115     c                   add       cosparzx      totcos2dx
192300030115     c                   add       cosparzx      totcos3dx
192400030115     c                   add       cosparzx      totcos4dx
192500030115      *competenze giorno in esame
192600030115     c                   add       comparzx      totcom1dx
192700030115     c                   add       comparzx      totcom2dx
192800030115     c                   add       comparzx      totcom3dx
192900030115     c                   add       comparzx      totcom4dx
193000030115      *costi giorno in esame solo consegne
193100030115     c                   add       conparzx      totcon1dx
193200030115     c                   add       conparzx      totcon2dx
193300030115     c                   add       conparzx      totcon3dx
193400030115     c                   add       conparzx      totcon4dx
193500030115      *costi giorno in esame solo ritiri
193600030115     c                   add       ritparzx      totrit1dx
193700030115     c                   add       ritparzx      totrit2dx
193800030115     c                   add       ritparzx      totrit3dx
193900030115     c                   add       ritparzx      totrit4dx
194000030115      *competenze giorno in esame solo consegne
194100030115     c                   add       conparzcx     totcon1dcx
194200030115     c                   add       conparzcx     totcon2dcx
194300030115     c                   add       conparzcx     totcon3dcx
194400030115     c                   add       conparzcx     totcon4dcx
194500030115      *competenze giorno in esame solo ritiri
194600030115     c                   add       ritparzcx     totrit1dcx
194700030115     c                   add       ritparzcx     totrit2dcx
194800030115     c                   add       ritparzcx     totrit3dcx
194900030115     c                   add       ritparzcx     totrit4dcx
195000030225     c                   endif
195100030115      * totalizzazione nel periodo
195200030225     c*                  else
195300030515      *stime periodo in esame
195400030515     c                   add       importo       totstm1px
195500030515     c                   add       importo       totstm2px
195600030515     c                   add       importo       totstm3px
195700030515     c                   add       importo       totstm4px
195800030115      *costi progressivo periodo
195900030115     c                   add       cosparzx      totcos1px
196000030115     c                   add       cosparzx      totcos2px
196100030115     c                   add       cosparzx      totcos3px
196200030115     c                   add       cosparzx      totcos4px
196300030115      *competenze progressivo periodo
196400030115     c                   add       comparzx      totcom1px
196500030115     c                   add       comparzx      totcom2px
196600030115     c                   add       comparzx      totcom3px
196700030115     c                   add       comparzx      totcom4px
196800030115      *costi periodo in esame solo consegne
196900030115     c                   add       conparzx      totcon1px
197000030115     c                   add       conparzx      totcon2px
197100030115     c                   add       conparzx      totcon3px
197200030115     c                   add       conparzx      totcon4px
197300030115      *costi periodo in esame solo ritiri
197400030115     c                   add       ritparzx      totrit1px
197500030115     c                   add       ritparzx      totrit2px
197600030115     c                   add       ritparzx      totrit3px
197700030115     c                   add       ritparzx      totrit4px
197800030115      *competenze periodo in esame solo consegne
197900030115     c                   add       conparzcx     totcon1pcx
198000030115     c                   add       conparzcx     totcon2pcx
198100030115     c                   add       conparzcx     totcon3pcx
198200030115     c                   add       conparzcx     totcon4pcx
198300030115      *competenze periodo in esame solo ritiri
198400030115     c                   add       ritparzcx     totrit1pcx
198500030115     c                   add       ritparzcx     totrit2pcx
198600030115     c                   add       ritparzcx     totrit3pcx
198700030115     c                   add       ritparzcx     totrit4pcx
198800030225     c*                  end
198900030115     c
199000030115     c                   enddo
199100030115     c                   endsr
199200030515      *------------------------------------------------------
199300030515     c     elaboraAFde   begsr
199400030515      *------------------------------------------------------
199500030515
199600030515      *loop record saldi coop
199700030522     c     kfss          setll     fifsa01l
199800030515     c                   do        *hival
199900030515     c                   read      fifsa01l                               98
200000030515     c   98              leave
200100030515      *rottura data o filiale
200200030515     c                   if        fsaddc > alladat or fsafgs <> orgfil
200300030515     c                   leave
200400030515     c                   endif
200500030515
200600030515      *totalizza stime aff/def  per giornata
200700151014      *verifica se filiale partita con nuove stime da tabella VPO
200800151014     c                   if        datavpo = 0 or dataVpo > fsaddc
200900030515     c                   move      *zeros        kfrepdr
201000030515     c                   movel     orgfil        kfrepdr
201100030515     c                   move      fsaddc        kfreddc
201200030515     c                   move      'D'           kfretsr
201300030526     c                   move      'G'           kfretip
201400030515     c                   clear                   importo
201500030515     c                   exsr      stime
201600151014     c                   else
201700151014     c*
201800151014     c                   movea     pDave         pvocA
201900151014     c                   movea     pDdar         pvocD
202000151014     c                   move      fsaddc        pdata
202100151014     c                   move      orgfil        pfil
202200151014     c                   move      'D'           ptsr
202300151014     c                   clear                   pimpo
202400151014     c                   call      'FICN34SR'
202500151014     c                   parm                    FICN34S
202600151014     c                   z-add     pimpo         importo
202700151014     c*
202800151014     c                   endif
202900080918     c* somma nei costi stimati eventuali aumenti costo carburante
203000080918     c                   add       fsaxx5        importo
203100030515      *totalizza i costi del record
203200030515     c                   eval      cosparza = (fsa014 + fsa015
203300030515     c                             + fsa046 + fsa047 + fsa049 + fsa050
203400080918     c                             + fsaxx1 + fsaxx2 + fsaxx3 + fsaxx4)
203500030515
203600030515      *totalizza le competenze del record
203700030515     c                   eval      comparza = (fsa014b + fsa015b
203800030515     c                             + fsa046b + fsa047b + fsa049b + fsa050b
203900080918     c                             + fsaxx1b + fsaxx2b + fsaxx3b + fsaxx4b)
204000030515
204100030515      *totalizza i costi solo consegne
204200030515     c                   eval      conparza = (fsa015 + fsa046 + fsa049)
204300030515
204400030515      *totalizza i costi solo ritiri
204500030515     c                   eval      ritparza = (fsa014 +fsa047 + fsa050)
204600030515
204700030515      *totalizza le competenze solo consegne
204800030515     c                   eval      conparzca = (fsa015b+ fsa046b+ fsa049b)
204900030515
205000030515      *totalizza le competenze solo ritiri
205100030515     c                   eval      ritparzca = (fsa014b + fsa046b + fsa050b)
205200030515
205300030515      *controllo  per sapere dove totalizzare se nel periodo o alla data
205400030526     c                   if        fsaddc >= alladatx
205500030515      *stime giorno in esame
205600030515     c                   add       importo       totstm1da
205700030515     c                   add       importo       totstm2da
205800030515     c                   add       importo       totstm3da
205900030515     c                   add       importo       totstm4da
206000030515      *costi giorno in esame
206100030515     c                   add       cosparza      totcos1da
206200030515     c                   add       cosparza      totcos2da
206300030515     c                   add       cosparza      totcos3da
206400030515     c                   add       cosparza      totcos4da
206500030515      *competenze giorno in esame
206600030515     c                   add       comparza      totcom1da
206700030515     c                   add       comparza      totcom2da
206800030515     c                   add       comparza      totcom3da
206900030515     c                   add       comparza      totcom4da
207000030515      *costi giorno in esame solo consegne
207100030515     c                   add       conparza      totcon1da
207200030515     c                   add       conparza      totcon2da
207300030515     c                   add       conparza      totcon3da
207400030515     c                   add       conparza      totcon4da
207500030515      *costi giorno in esame solo ritiri
207600030515     c                   add       ritparza      totrit1da
207700030515     c                   add       ritparza      totrit2da
207800030515     c                   add       ritparza      totrit3da
207900030515     c                   add       ritparza      totrit4da
208000030515      *competenze giorno in esame solo consegne
208100030515     c                   add       conparzca     totcon1dca
208200030515     c                   add       conparzca     totcon2dca
208300030515     c                   add       conparzca     totcon3dca
208400030515     c                   add       conparzca     totcon4dca
208500030515      *competenze giorno in esame solo ritiri
208600030515     c                   add       ritparzca     totrit1dca
208700030515     c                   add       ritparzca     totrit2dca
208800030515     c                   add       ritparzca     totrit3dca
208900030515     c                   add       ritparzca     totrit4dca
209000030515     c                   endif
209100030515      * totalizzazione nel periodo
209200030515     c*                  else
209300030515      *stime progressivo in esame
209400030515     c                   add       importo       totstm1pa
209500030515     c                   add       importo       totstm2pa
209600030515     c                   add       importo       totstm3pa
209700030515     c                   add       importo       totstm4pa
209800030515      *costi progressivo periodo
209900030515     c                   add       cosparza      totcos1pa
210000030515     c                   add       cosparza      totcos2pa
210100030515     c                   add       cosparza      totcos3pa
210200030515     c                   add       cosparza      totcos4pa
210300030515      *competenze progressivo periodo
210400030515     c                   add       comparza      totcom1pa
210500030515     c                   add       comparza      totcom2pa
210600030515     c                   add       comparza      totcom3pa
210700030515     c                   add       comparza      totcom4pa
210800030515      *costi periodo in esame solo consegne
210900030515     c                   add       conparza      totcon1pa
211000030515     c                   add       conparza      totcon2pa
211100030515     c                   add       conparza      totcon3pa
211200030515     c                   add       conparza      totcon4pa
211300030515      *costi periodo in esame solo ritiri
211400030515     c                   add       ritparza      totrit1pa
211500030515     c                   add       ritparza      totrit2pa
211600030515     c                   add       ritparza      totrit3pa
211700030515     c                   add       ritparza      totrit4pa
211800030515      *competenze periodo in esame solo consegne
211900030515     c                   add       conparzca     totcon1pca
212000030515     c                   add       conparzca     totcon2pca
212100030515     c                   add       conparzca     totcon3pca
212200030515     c                   add       conparzca     totcon4pca
212300030515      *competenze periodo in esame solo ritiri
212400030515     c                   add       ritparzca     totrit1pca
212500030515     c                   add       ritparzca     totrit2pca
212600030515     c                   add       ritparzca     totrit3pca
212700030515     c                   add       ritparzca     totrit4pca
212800030515     c*                  end
212900030515     c
213000030515     c                   enddo
213100030515     c                   endsr
213200030515      *------------------------------------------------------
213300030515     c     stime         begsr
213400030515      *------------------------------------------------------
213500030515      * esegue totalizzazione stime per tipologia
213600030515     c     kfre          setll     fifre02l
213700030515     c                   do        *hival
213800030515     c     kfre          reade     fifre02l                               97
213900030515     c   97              leave
214000030515     c                   add       fretim        importo
214100030515     c                   enddo
214200030515     c                   endsr
214300020521      *------------------------------------------------------
214400020611     c     invia         begsr
214500020521      *------------------------------------------------------
214600020612      *deve eseguire l'invio nelle code apposite
214700020612
214800020612     c                   select
214900020612     c                   when      livello = 3
215000020612     c                   close     ecoprt3
215100020612     c                   close     ecoprt2
215200020612     c                   close     ecoprt1
215300020612     c                   when      livello = 2
215400020612     c                   close     ecoprt2
215500020612     c                   close     ecoprt1
215600020612     c                   when      livello = 1
215700020612     c                   close     ecoprt1
215800020612     c                   endsl
215900020521
216000020521     c                   endsr
216100020611      *------------------------------------------------------
216200020611     c     imbuca        begsr
216300020611      *------------------------------------------------------
216400020612      *deve overridare i printer con le nuove indicazioni di istradamento
216500020612     c                   select
216600020612      *rottura distretto
216700020612     c                   when      livello = 3
216800020612      * override per distretto
216900020612     c                   z-add     3             lvl
217000020619      *reperimento coda output
217100020619     c                   move      '17'          tblcod
217200030227     c                   movel(p)  exdistr       tblkey
217300020619     c     ktab          chain     tabel00f                           90
217400020619     c                   movel     tbluni        ds17
217500030227     c                   movel     §17des        exdesd           15
217600020619      *imposta per override
217700020619     c                   movel     §17otq        PrmOutq
217800020612     c                   exsr      openovr
217900020612     c                   open      ecoprt3
218000020612      * override per area
218100020612     c                   z-add     2             lvl
218200020619      *reperimento coda output
218300020619     c                   move      '05'          tblcod
218400020619     c                   movel     exarea        tblkey
218500020619     c     ktab          chain     tabel00f                           90
218600020619     c                   movel     tbluni        ds05
218700030227     c                   movel     §05des        exdesa           18
218800020619      *imposta per override
218900020619     c                   movel     §05otq        PrmOutq
219000020612     c                   exsr      openovr
219100020612     c                   open      ecoprt2
219200020612      * override per P.O.
219300020612     c                   z-add     1             lvl
219400030513     c                   if        orgfag = 'A'
219500030513     c                   move      outqsede      prmoutq
219600030513     c                   else
219700020619     c                   movel     orgde4        PrmOutq
219800030513     c                   endif
219900020612     c                   exsr      openovr
220000020612     c                   open      ecoprt1
220100020612      *rottura area
220200020612     c                   when      livello = 2
220300020612      * override per area
220400020612     c                   z-add     2             lvl
220500020619      *reperimento coda output
220600020619     c                   move      '05'          tblcod
220700020619     c                   movel     exarea        tblkey
220800020619     c     ktab          chain     tabel00f                           90
220900020619     c                   movel     tbluni        ds05
221000030227     c                   movel     §05des        exdesa
221100020619      *imposta per override
221200020619     c                   movel     §05otq        PrmOutq
221300020612     c                   exsr      openovr
221400020612     c                   open      ecoprt2
221500020612      * override per P.O.
221600020612     c                   z-add     1             lvl
221700030513     c                   if        orgfag = 'A'
221800030513     c                   move      outqsede      prmoutq
221900030513     c                   else
222000020619     c                   movel     orgde4        PrmOutq
222100030513     c                   endif
222200020612     c                   exsr      openovr
222300020612     c                   open      ecoprt1
222400020612      *rottura area
222500020612     c                   when      livello = 1
222600020612      * override per P.O.
222700020612     c                   z-add     1             lvl
222800030513     c                   if        orgfag = 'A'
222900030513     c                   move      outqsede      prmoutq
223000030513     c                   else
223100020619     c                   movel     orgde4        PrmOutq
223200030513     c                   endif
223300020612     c                   exsr      openovr
223400020612     c                   open      ecoprt1
223500020612
223600020612     c                   endsl
223700020611
223800020611     c                   endsr
223900030922     C*----------------------------------------------------*
224000030922     C     riepiloghi    begsr
224100030922     C*----------------------------------------------------*
224200030922     c                   move      exfil         fsrfgs
224300031006     c                   z-add     alladat       fsrddc
224400030922     c                   move      *blank        fsrflr
224500030922      *record autotrasportatori
224600030922     c                   move      'A'           fsrtpr
224700030922     c                   z-add     totstm1d      fsrstg
224800030922     c                   z-add     totcos1d      fsrcog
224900030922     c                   z-add     totcom1d      fsrcmg
225000030922     c                   z-add     totstm1p      fsrstp
225100030922     c                   z-add     totcos1p      fsrcop
225200030922     c                   z-add     totcom1p      fsrcmp
225300030922     c                   write     fifsr000
225400030922      *record coop
225500030922     c                   move      'C'           fsrtpr
225600030922     c                   z-add     totstm1dx     fsrstg
225700030922     c                   z-add     totcos1dx     fsrcog
225800030922     c                   z-add     totcom1dx     fsrcmg
225900030922     c                   z-add     totstm1px     fsrstp
226000030922     c                   z-add     totcos1px     fsrcop
226100030922     c                   z-add     totcom1px     fsrcmp
226200030922     c                   write     fifsr000
226300030922      *record aff/Def
226400030922     c                   move      'D'           fsrtpr
226500030922     c                   z-add     totstm1da     fsrstg
226600030922     c                   z-add     totcos1da     fsrcog
226700030922     c                   z-add     totcom1da     fsrcmg
226800030922     c                   z-add     totstm1pa     fsrstp
226900030922     c                   z-add     totcos1pa     fsrcop
227000030922     c                   z-add     totcom1pa     fsrcmp
227100030922     c                   write     fifsr000
227200030922     c                   endsr
227300020612     C*----------------------------------------------------*
227400020612     C     openovr       begsr
227500020612     C*----------------------------------------------------*
227600020620     C                   z-add     62            lung             15 5
227700020620     C                   clear                   comman
227800020612      *ovr su outq di filiale
227900030403     c                   if        dOvr = 'N'
228000030403     C                   EVAL      %SUBST(cmd:52:10) = OutqSede
228100030403     c                   else
228200020620     C                   EVAL      %SUBST(cmd:52:10) = PrmOutq
228300030403     c                   end
228400020612     C                   movea     cmd(lvl)      comman           80
228500020612     C                   call      'QCMDEXC'                            20
228600020612     C                   parm                    comman
228700020612     C                   parm                    lung
228800020612     C*
228900020612     c                   endsr
229000020619     C*----------------------------------------------------*
229100020619     C     testate       begsr
229200020619     C*----------------------------------------------------*
229300020619     c                   if        lin4 > 60
229400020619     c                   except    testa4
229500020619     c                   end
229600020619
229700020619     c                   if        lin3 > 60
229800020619     c                   except    testa3
229900020619     c                   end
230000020619
230100020619     c                   if        lin2 > 60
230200020619     c                   except    testa2
230300020619     c                   end
230400020619
230500020619     c                   endsr
230600050506      *-------------------------------------------------
230700050506     c     pulriep       begsr
230800050506      *-------------------------------------------------
230900050506     C                   z-add     62            lung
231000050506     C                   clear                   comman
231100050506     C                   movea     cmd(5)        comman           80
231200050506     C                   call      'QCMDEXC'                            20
231300050506     C                   parm                    comman
231400050506     C                   parm                    lung
231500050506     c                   move      'X'           schedulato
231600050506     c                   open      fifsr00f
231700050506     c                   endsr
231800020521      *-------------------------------------------------
231900020521     c     *inzsr        begsr
232000020521      *-------------------------------------------------
232100020612     c     *entry        plist
232200020612     c                   parm                    kpjba
232300170419      *
232400170419     c                   clear                   memorizza_QLK     1
232500170419      *
232600170419      *  Chiamato da lancio schedulato
232700030225     c                   if        kpjbu = *blank
232800170419     c                   eval        memorizza_QLK ='S'
232900030225     c                   clear                   param
233000050506      * in caso di lancio schedulato pulisco il file riepiloghi
233100050506     c                   exsr      pulriep
233200170419      *
233300030225     c                   else
233400030225     c                   movel     kpjbu         param
233500170419      *
233600051123      * pulizia file riepiloghi x lancio extra fine mese sab/dom
233700051123     c                   if        riepcdg= 'S'
233800051123     c                   exsr      pulriep
233900051123     c                   endif
234000030225     c                   end
234100170419      *
234200030226     c                   movea     dist          dista            10
234300030225      * se non trovo i parametri impostati sono in schedulazione quindi
234400050506      * ricavo i parametri per l'elaborazione a meno che non ci sia
234500050506      * la forzatura dell'udate x simulare la schedulazione forzata
234600050506
234700030227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
234800030227     C***
234900030227     C                   TIME                    W0120            14 0
235000030227     C                   MOVEl     W0120         Wora              6 0
235100030227     C                   MOVE      W0120         WDAT              8 0
235200170419      *
235300050506      * verifico che non sia forzata la udate per la rigenerazione
235400050506      * del file riepiloghi per il controllo di gestione se la data
235500050506      * è valorizzata il lancio è forzato e non schedulato
235600050506     c                   if        frigen = 'S'
235700050506     c                   exsr      pulriep
235800050506     c                   z-add     fudate        wdat
235900050506     c                   endif
236000170419     C*
236100030225     c                   if        alladat = 0
236200030225     C*
236300030225     C                   Z-ADD     WDAT          G02DAT
236400030225     C                   MOVEL     *BLANK        G02ERR
236500030225     C                   CALL      'XSRDA8'
236600030225     C                   PARM                    WLBDAT
236700030225     C* UDATE A 8 IN AAAA/MM/GG
236800030225     C                   move      G02INV        dataiso
236900030225      *controllo se udate è festivo non lancio l'elaborazione
237000030225     c                   move      dataiso       dadata
237100030225     c                   move      dataiso       adata
237200030225     c                   CALL      'XSRLAV8'
237300030225     c                   PARM                    Wdata8
237400170419      *
237500030225     c                   if        giolav = 0
237600051124      * RICHIAMO PGM PER INVIO EVENTUALE MAIL SE FINE MESE SAB/DOM
237700051124      * AVVISA CON MAIL DI LANCIARE MANUALMENTE IL C.E. GIORNALIERO
237800051124     c                   call      'FICN34R1'
237900151016     c                   parm                    ficn34v
238000030225     c                   seton                                        lr
238100030225     c                   endif
238200170419      *
238300030225      *reperisco limiti da tabella per calcolare il D-Day
238400030225     c                   movel(p)  '1'           tbeke1
238500030225     c                   movel     'CCG'         tbecod
238600030225     c     ktbe          chain     tntbe01l                           90
238700030225     c                   if        not *in90
238800030225     c                   movel     tbeuni        dccg
238900030225     c                   endif
239000170419      *
239100030225      * calcolo il D-Day partendo da udate cercando i giorni lavorativi
239200030225      * all'indietro a seconda dei giorni impostati in tabella
239300030225     c     dataiso       subdur    1:*d          dataiso2
239400030225     c                   do        *hival
239500030225     c                   move      dataiso2      dadata
239600030225     c                   move      dataiso       adata
239700030225     c                   CALL      'XSRLAV8'
239800030225     c                   PARM                    Wdata8
239900030225      * se corrisponde ho trovato il D-Day
240000030225     c                   if        giolav = §cggce
240100030225     c                   leave
240200030225     c                   else
240300030225      * altrimenti continuo a sottrarre i giorni da udate
240400030225     c     dataiso2      subdur    1:*d          dataiso2
240500030225     c                   iter
240600030225     c                   endif
240700030225     c                   enddo
240800170419      *
240900030225      * ho stabilito il D-DAY
241000030225     c                   move      dataiso2      alladat
241100030225     c                   move      alladat       dalladat
241200030225     c                   move      01            dalladat
241300170419      *--
241400170419      *   se  chiamato da schedulazione deve impostare la data di elaborazione
241500170419      *   sul file di tracciabilità del B.I. AZQLK00F
241600170419      *--
241700170419     c                   if          memorizza_QLK ='S'
241800170419     c                   movel     *all'0'       timestp_adat     14
241900170419     c                   move      wora          timestp_adat
242000170419     c                   movel     alladat       timestp_adat
242100170419      *
242200170419      ***?Aggancio il file con il tipo elaborazione passato
242300170419     c                   movel(p)  'FICN34R'     QLKELA
242400170419     c     QLKELA        chain     AZQLK01L
242500170419     c                   eval      QLKtck = timestp_adat
242600170419     c                   clear                   QLKqlk
242700170419     c                   if        %Found(AZQLK01L)
242800170419     c                   update    AZQLK000
242900170419     c                   else
243000170419     c                   movel(p)  'FICN34R'     QLKELA
243100170419     c                   write     AZQLK000
243200170419     c                   end
243300170419      *
243400170419     c                   endif
243500030225
243600030225     c                   else
243700030225
243800030225      *lancio da video parto da qui con data fine periodo già impostata
243900030225     c                   move      alladat       dataiso2
244000030225     c                   endif
244100030225      * verifico se i giorni precedenti al D-Day sono festivi per accorparli
244200030225     c     dataiso2      subdur    1:*d          dataiso3
244300030225     c                   do        *hival
244400030225     c                   move      dataiso3      dadata
244500030225     c                   move      dataiso2      adata
244600030225     c                   CALL      'XSRLAV8'
244700030225     c                   PARM                    Wdata8
244800030225      * quando i giorni lavorativi diventano 2 vuol dire che ho chiuso il
244900030225      * sottoperiodo di raggruppamento del D-DAY
245000030225      * esempio: se il D-DAY è lunedi il sabato e domenica precedenti
245100030225      * vanno sommati assieme al D-Day nella stampa
245200051026      * NEW- questo avviene solo se non ci troviamo a cavallo di mese
245300051026      * nell'esempio sopra riportato
245400051026      *__________ verifico se il mese delle date in esame è uguale _______
245500051026     c                   extrct    dataiso2:*m   mese1             2 0
245600051026     c                   extrct    dataiso3:*m   mese2             2 0
245700030225
245800030225      * se trovo il secondo giorno lavorativo aggiungo uno per tornare al
245900030225      * giorno seguente (calendario) che è la data di partenza del periodo
246000030225      * del D-day
246100051026     c                   if        giolav = 2 or mese1 <> mese2
246200030225     c     dataiso3      adddur    1:*d          dataiso3
246300030225     c                   leave
246400030225     c                   else
246500030225      * altrimenti continuo a sottrarre i giorni da udate
246600030225     c     dataiso3      subdur    1:*d          dataiso3
246700030225     c                   iter
246800030225     c                   endif
246900030225     c                   enddo
247000030225      * ho stabilito eventuale ragguppamento del D-DAY
247100030225     c                   move      dataiso3      alladatx
247200020612
247300020611     c                   z-add     66            lin1
247400020611     c                   z-add     66            lin2
247500020611     c                   z-add     66            lin3
247600020619     c                   z-add     66            lin4
247700020619     c                   z-add     3             livello
247800030508      *ovr per ecoprt4 report globale di sede
247900030508     C                   z-add     62            lung
248000030508     C                   clear                   comman
248100030508     C                   movea     cmd(4)        comman           80
248200030603     c                   if        dOvr = 'N'
248300030603     C                   EVAL      %SUBST(comman:52:10) = OutqSede
248400030603     c                   end
248500030508     C                   call      'QCMDEXC'                            20
248600030508     C                   parm                    comman
248700030508     C                   parm                    lung
248800030508     c                   open      ecoprt4
248900030508      *
249000020611     c     kfss          klist
249100020611     c                   kfld                    orgfil
249200020611     c                   kfld                    dalladat
249300030225
249400020619     c     ktab          klist
249500020619     c                   kfld                    tblkut
249600020619     c                   kfld                    tblcod
249700020619     c                   kfld                    tblkey
249800020619     c                   z-add     1             tblkut
249900020619      *
250000030225     c     ktbe          klist
250100030225     c                   kfld                    tbecod
250200030225     c                   kfld                    tbeke1
250300151014      *
250400151014     c     ktbe2         klist
250500151014     c                   kfld                    tbecod
250600151014     c                   kfld                    tbeke1
250700151014     c                   kfld                    tbeke2
250800030225      *
250900030515     c     kfre          klist
251000030515     c                   kfld                    kfretip           1
251100030515     c                   kfld                    kfrepdr           7 0
251200030515     c                   kfld                    kfreddc           8 0
251300030515     c                   kfld                    kfretsr           1
251400030522      *
251500020619     c                   move      alladat       dataiso
251600020619     c                   move      dataiso       dataeur
251700020619     c                   move      dataeur       alladatg
251800030225
251900020619     c                   move      dalladat      dataiso
252000020619     c                   move      dataiso       dataeur
252100020619     c                   move      dataeur       dalladatg
252200020521
252300151014      * reperisce le voci da cercare in contabilità per le stime di sede
252400151014     c                   call      'FICN34VR'
252500151016     c                   parm                    ficn34v
252600151014      *
252700151015     c*m                 movea     pDave         Dave
252800151015     c*m                 movea     pDdar         Ddar
252900151015     c*m                 movea     pAave         Aave
253000151015     c*m                 movea     pAdar         Adar
253100151015     c*m                 movea     pcave         cave
253200151015     c*m                 movea     pcdar         cdar
253300151014     c*
253400020521     c                   endsr
253500151014     c*____________________________________________________________
253600151014     c     repVpo        begsr
253700151014     c*____________________________________________________________
253800151014     c                   clear                   dataVpo           8 0
253900151015      *reperisco limiti da tabella per switch su nuove stime
254000151014     c                   movel(p)  'FICN34R'     tbeke1
254100151014     c                   movel(p)  orgfil        tbeke2
254200151014     c                   movel     'VPO'         tbecod
254300151014     c     ktbe2         chain     tntbe01l
254400151014     c                   if        %found(tntbe01l)
254500151014     c                   movel     tbeuni        dataVpo
254600151014     C                   Z-ADD     dataVpo       G02inv
254700151014     C                   MOVEL     '3'           G02ERR
254800151014     C                   CALL      'XSRDA8'
254900151014     C                   PARM                    WLBDAT
255000151016     c                   if        g02err <> '0'
255100151014     c                   move      *zeros        dataVpo
255200151014     c                   end
255300151014     c                   else
255400151014      *ricerco 999 abilitazione per tutti
255500151014     c                   movel(p)  '999'         tbeke2
255600151014     c                   movel     'VPO'         tbecod
255700151014     c     ktbe2         chain     tntbe01l
255800151014     c                   if        %found(tntbe01l)
255900151014     c                   movel     tbeuni        dataVpo
256000151014     C                   Z-ADD     dataVpo       G02inv
256100151014     C                   MOVEL     '3'           G02ERR
256200151014     C                   CALL      'XSRDA8'
256300151014     C                   PARM                    WLBDAT
256400151016     c                   if        g02err <> '0'
256500151014     c                   move      *zeros        dataVpo
256600151014     c                   end
256700151014     c                   endif
256800151014     c                   endif
256900151014     c                   endsr
257000020517     O*------------------------------------------------------*
257100020619     Oecoprt1   e            TESTA1            2
257200020625     o                                            8 'FICN34R'
257300020625     o                       *date         y   +  3
257400030227     o                       wora              +  2 '  :  :  '
257500030319     O                                          100 'Stampa C.E. giornaliero -
257600030314     O                                              Costi variabili'
257700030319     o                                          110 'Dal '
257800020619     o                       dalladatg         +  2 '  /  /    '
257900020620     o                                         +  2 'Al '
258000020619     o                       alladatg          +  2 '  /  /    '
258100030314     o                                         +  2 'Per Punto Operativo'
258200030314     O          e            TESTA1      0
258300030319     O                                          100 'Stampa C.E. giornaliero -
258400030314     O                                              Costi variabili'
258500030319     o                                          110 'Dal '
258600020625     o                       dalladatg         +  2 '  /  /    '
258700020625     o                                         +  2 'Al '
258800020625     o                       alladatg          +  2 '  /  /    '
258900020625     o                                         +  2 'Per Punto Operativo'
259000020625     O          e            TESTA1      0
259100030319     O                                          100 'Stampa C.E. giornaliero -
259200030314     O                                              Costi variabili'
259300030319     o                                          110 'Dal '
259400020625     o                       dalladatg         +  2 '  /  /    '
259500020625     o                                         +  2 'Al '
259600020625     o                       alladatg          +  2 '  /  /    '
259700020625     o                                         +  2 'Per Punto Operativo'
259800020619     O          e            testa1      2
259900020620     O                                           12 'P.Operativo'
260000020620     O                                           80 'Giornata del'
260100020625     o                       alladatg            95 '  /  /    '
260200020620     O                                          150 'Progressivo al'
260300020620     o                       alladatg           165 '  /  /    '
260400020625     O          e            testa1      0
260500020625     O                                           12 'P.Operativo'
260600020625     O                                           80 'Giornata del'
260700020625     o                       alladatg            95 '  /  /    '
260800020625     O                                          150 'Progressivo al'
260900020625     o                       alladatg           165 '  /  /    '
261000020625     O          e            testa1      0
261100020625     O                                           12 'P.Operativo'
261200020625     O                                           80 'Giornata del'
261300020625     o                       alladatg            95 '  /  /    '
261400020625     O                                          150 'Progressivo al'
261500020625     o                       alladatg           165 '  /  /    '
261600020619     O          e            testa1      1
261700060913     o                                           47 'C.Stimati'
261800030528     o                                           60 'Costi'
261900030528     o                                           74 'Costi'
262000030528     o                                           88 'Competenze'
262100030528     o                                          110 'Scostamento'
262200020619      *
262300060913     o                                          130 'C.Stimati'
262400030528     o                                          142 'Costi'
262500030528     o                                          156 'Costi'
262600030528     o                                          170 'Competenze'
262700030528     o                                          192 'Scostamento'
262800020625     O          e            testa1      0
262900060913     o                                           47 'C.Stimati'
263000030528     o                                           60 'Costi'
263100030528     o                                           74 'Costi'
263200030528     o                                           88 'Competenze'
263300030528     o                                          110 'Scostamento'
263400030528      *
263500060913     o                                          130 'C.Stimati'
263600030528     o                                          142 'Costi'
263700030528     o                                          156 'Costi'
263800030528     o                                          170 'Competenze'
263900030528     o                                          192 'Scostamento'
264000020625     O          e            testa1      0
264100060913     o                                           47 'C.Stimati'
264200030528     o                                           60 'Costi'
264300030528     o                                           74 'Costi'
264400030528     o                                           88 'Competenze'
264500030528     o                                          110 'Scostamento'
264600030528      *
264700060913     o                                          130 'C.Stimati'
264800030528     o                                          142 'Costi'
264900030528     o                                          156 'Costi'
265000030528     o                                          170 'Competenze'
265100030528     o                                          192 'Scostamento'
265200020619     O          e            testa1      1
265300060913     o                                           48 'Rettifiche'
265400030528     o                                           60 'Reali'
265500030528     o                                           74 'Totali'
265600030528     o                                          104 'Valore'
265700030203     o                                          116 'Delta %'
265800020619      *
265900060913     o                                          130 'Rettifiche'
266000030528     o                                          142 'Reali'
266100030528     o                                          156 'Totali'
266200030528     o                                          186 'Valore'
266300030522     o                                          198 'Delta %'
266400020625     O          e            testa1      0
266500060913     o                                           48 'Rettifiche'
266600030528     o                                           60 'Reali'
266700030528     o                                           74 'Totali'
266800030528     o                                          104 'Valore'
266900030528     o                                          116 'Delta %'
267000030528      *
267100060913     o                                          130 'Rettifiche'
267200030528     o                                          142 'Reali'
267300030528     o                                          156 'Totali'
267400030528     o                                          186 'Valore'
267500030528     o                                          198 'Delta %'
267600020625     O          e            testa1      0
267700060913     o                                           48 'Rettifiche'
267800030528     o                                           60 'Reali'
267900030528     o                                           74 'Totali'
268000030528     o                                          104 'Valore'
268100030528     o                                          116 'Delta %'
268200030528      *
268300060913     o                                          130 'Rettifiche'
268400030528     o                                          142 'Reali'
268500030528     o                                          156 'Totali'
268600030528     o                                          186 'Valore'
268700030528     o                                          198 'Delta %'
268800020619     O          e            detta       1
268900020620     O                       exfil                3
269000020620     o                       desfil              20
269100030527     o                                           30 'Aut.'
269200060913     o                       totstm1ds     j     46
269300030528     o                       totcos1ds     2     60
269400060913     o                       totsco1s      j     74
269500030528     o                       totcom1ds     2     88
269600030528     o                       totdif1s      j    104
269700030528     o                       totper1s      j    116
269800030528      *
269900060913     o                       totstm1ps     j    128
270000030528     o                       totcos1ps     2    142
270100060913     o                       totsco1ps     j    156
270200030528     o                       totcom1ps     2    170
270300030528     o                       totdif1ps     j    186
270400030528     o                       totper1ps     j    198
270500030115     O          e            detta       1
270600030527     o                                           30 'Coop'
270700060913     o                       totstm1dsx    j     46
270800030528     o                       totcos1dsx    2     60
270900060913     o                       totsco1sx     j     74
271000030528     o                       totcom1dsx    2     88
271100030528     o                       totdif1sx     j    104
271200030528     o                       totper1sx     j    116
271300030528     o
271400060913     o                       totstm1psx    j    128
271500030528     o                       totcos1psx    2    142
271600060913     o                       totsco1psx    j    156
271700030528     o                       totcom1psx    2    170
271800030528     o                       totdif1psx    j    186
271900030528     o                       totper1psx    j    198
272000030515     O          e            detta       1
272100030527     o                                           30 'AfDe'
272200060913     o                       totstm1dsa    j     46
272300030528     o                       totcos1dsa    2     60
272400060913     o                       totsco1sa     j     74
272500030528     o                       totcom1dsa    2     88
272600030528     o                       totdif1sa     j    104
272700030528     o                       totper1sa     j    116
272800030528     o
272900060913     o                       totstm1psa    j    128
273000030528     o                       totcos1psa    2    142
273100060913     o                       totsco1psa    j    156
273200030528     o                       totcom1psa    2    170
273300030528     o                       totdif1psa    j    186
273400030528     o                       totper1psa    j    198
273500030528     O          e            detta       1
273600030528     o                                           30 'Tot.'
273700060913     o                       stime1        j     46
273800030528     o                       reali1        2     60
273900060913     o                       costi1        j     74
274000030528     o                       compe1        2     88
274100030528     o                       diffe1        j    104
274200030528     o                       tpcompe1      j    116
274300030528     o
274400060913     o                       stime1p       j    128
274500030528     o                       reali1p       2    142
274600060913     o                       costi1p       j    156
274700030528     o                       compe1p       2    170
274800030528     o                       diffe1p       j    186
274900030528     o                       tpcompe1p     j    198
275000030528     O          e            detta       0
275100030528     o                                           30 'Tot.'
275200060913     o                       stime1        j     46
275300030528     o                       reali1        2     60
275400060913     o                       costi1        j     74
275500030528     o                       compe1        2     88
275600030528     o                       diffe1        j    104
275700030528     o                       tpcompe1      j    116
275800030528     o
275900060913     o                       stime1p       j    128
276000030528     o                       reali1p       2    142
276100060913     o                       costi1p       j    156
276200030528     o                       compe1p       2    170
276300030528     o                       diffe1p       j    186
276400030528     o                       tpcompe1p     j    198
276500030528     O          e            detta       0
276600030528     o                                           30 'Tot.'
276700060913     o                       stime1        j     46
276800030528     o                       reali1        2     60
276900060913     o                       costi1        j     74
277000030528     o                       compe1        2     88
277100030528     o                       diffe1        j    104
277200030528     o                       tpcompe1      j    116
277300030528     o
277400060913     o                       stime1p       j    128
277500030528     o                       reali1p       2    142
277600060913     o                       costi1p       j    156
277700030528     o                       compe1p       2    170
277800030528     o                       diffe1p       j    186
277900030528     o                       tpcompe1p     j    198
278000020619     Oecoprt2   e            TESTA2            2
278100020625     o                                            8 'FICN34R'
278200020625     o                       *date         y   +  3
278300030227     o                       wora              +  2 '  :  :  '
278400030319     O                                          100 'Stampa C.E. giornaliero -
278500030314     O                                              Costi variabili'
278600030319     o                                          110 'Dal '
278700020619     o                       dalladatg         +  2 '  /  /    '
278800020620     o                                         +  2 'Al '
278900020619     o                       alladatg          +  2 '  /  /    '
279000020625     o                                         +  2 'Per Area'
279100020625     O          e            TESTA2      0
279200030319     O                                          100 'Stampa C.E. giornaliero -
279300030314     O                                              Costi variabili'
279400030319     o                                          110 'Dal '
279500020625     o                       dalladatg         +  2 '  /  /    '
279600020625     o                                         +  2 'Al '
279700020625     o                       alladatg          +  2 '  /  /    '
279800020625     o                                         +  2 'Per Area'
279900020625     O          e            TESTA2      0
280000030319     O                                          100 'Stampa C.E. giornaliero -
280100030314     O                                              Costi variabili'
280200030319     o                                          110 'Dal '
280300020625     o                       dalladatg         +  2 '  /  /    '
280400020625     o                                         +  2 'Al '
280500020625     o                       alladatg          +  2 '  /  /    '
280600020625     o                                         +  2 'Per Area'
280700020619     O          e            testa2      2
280800020620     O                                           12 'P.Operativo'
280900020620     O                                           80 'Giornata del'
281000020620     o                       alladatg            95 '  /  /    '
281100020620     O                                          150 'Progressivo al'
281200020620     o                       alladatg           165 '  /  /    '
281300020625     O          e            testa2      0
281400020625     O                                           12 'P.Operativo'
281500020625     O                                           80 'Giornata del'
281600020625     o                       alladatg            95 '  /  /    '
281700020625     O                                          150 'Progressivo al'
281800020625     o                       alladatg           165 '  /  /    '
281900020625     O          e            testa2      0
282000020625     O                                           12 'P.Operativo'
282100020625     O                                           80 'Giornata del'
282200020625     o                       alladatg            95 '  /  /    '
282300020625     O                                          150 'Progressivo al'
282400020625     o                       alladatg           165 '  /  /    '
282500020619     O          e            testa2      1
282600060913     o                                           47 'C.Stimati'
282700030528     o                                           60 'Costi'
282800030528     o                                           74 'Costi'
282900030528     o                                           88 'Competenze'
283000030528     o                                          110 'Scostamento'
283100030528      *
283200060913     o                                          130 'C.Stimati'
283300030528     o                                          142 'Costi'
283400030528     o                                          156 'Costi'
283500030528     o                                          170 'Competenze'
283600030528     o                                          192 'Scostamento'
283700020625     O          e            testa2      0
283800060913     o                                           47 'C.Stimati'
283900030528     o                                           60 'Costi'
284000030528     o                                           74 'Costi'
284100030528     o                                           88 'Competenze'
284200030528     o                                          110 'Scostamento'
284300030528      *
284400060913     o                                          130 'C.Stimati'
284500030528     o                                          142 'Costi'
284600030528     o                                          156 'Costi'
284700030528     o                                          170 'Competenze'
284800030528     o                                          192 'Scostamento'
284900020625     O          e            testa2      0
285000060913     o                                           47 'C.Stimati'
285100030528     o                                           60 'Costi'
285200030528     o                                           74 'Costi'
285300030528     o                                           88 'Competenze'
285400030528     o                                          110 'Scostamento'
285500030528      *
285600060913     o                                          130 'C.Stimati'
285700030528     o                                          142 'Costi'
285800030528     o                                          156 'Costi'
285900030528     o                                          170 'Competenze'
286000030528     o                                          192 'Scostamento'
286100020619     O          e            testa2      1
286200060913     o                                           48 'Rettifiche'
286300030528     o                                           60 'Reali'
286400030528     o                                           74 'Totali'
286500030528     o                                          104 'Valore'
286600030528     o                                          116 'Delta %'
286700030528      *
286800060913     o                                          130 'Rettifiche'
286900030528     o                                          142 'Reali'
287000030528     o                                          156 'Totali'
287100030528     o                                          186 'Valore'
287200030528     o                                          198 'Delta %'
287300020625     O          e            testa2      0
287400060913     o                                           48 'Rettifiche'
287500030528     o                                           60 'Reali'
287600030528     o                                           74 'Totali'
287700030528     o                                          104 'Valore'
287800030528     o                                          116 'Delta %'
287900030528      *
288000060913     o                                          130 'Rettifiche'
288100030528     o                                          142 'Reali'
288200030528     o                                          156 'Totali'
288300030528     o                                          186 'Valore'
288400030528     o                                          198 'Delta %'
288500020625     O          e            testa2      0
288600060913     o                                           48 'Rettifiche'
288700030528     o                                           60 'Reali'
288800030528     o                                           74 'Totali'
288900030528     o                                          104 'Valore'
289000030528     o                                          116 'Delta %'
289100030528      *
289200060913     o                                          130 'Rettifiche'
289300030528     o                                          142 'Reali'
289400030528     o                                          156 'Totali'
289500030528     o                                          186 'Valore'
289600030528     o                                          198 'Delta %'
289700020619     O          e            detta       1
289800020620     O                       exfil                3
289900020620     o                       desfil              20
290000030527     o                                           30 'Aut.'
290100060913     o                       totstm1ds     j     46
290200030528     o                       totcos1ds     2     60
290300060913     o                       totsco1s      j     74
290400030528     o                       totcom1ds     2     88
290500030528     o                       totdif1s      j    104
290600030528     o                       totper1s      j    116
290700030528     o
290800060913     o                       totstm1ps     j    128
290900030528     o                       totcos1ps     2    142
291000060913     o                       totsco1ps     j    156
291100030528     o                       totcom1ps     2    170
291200030528     o                       totdif1ps     j    186
291300030528     o                       totper1ps     j    198
291400030115     O          e            detta       1
291500030527     o                                           30 'Coop'
291600060913     o                       totstm1dsx    j     46
291700030528     o                       totcos1dsx    2     60
291800060913     o                       totsco1sx     j     74
291900030528     o                       totcom1dsx    2     88
292000030528     o                       totdif1sx     j    104
292100030528     o                       totper1sx     j    116
292200030528     o
292300060913     o                       totstm1psx    j    128
292400030528     o                       totcos1psx    2    142
292500060913     o                       totsco1psx    j    156
292600030528     o                       totcom1psx    2    170
292700030528     o                       totdif1psx    j    186
292800030528     o                       totper1psx    j    198
292900030515     O          e            detta       1
293000030527     o                                           30 'AfDe'
293100060913     o                       totstm1dsa    j     46
293200030528     o                       totcos1dsa    2     60
293300060913     o                       totsco1sa     j     74
293400030528     o                       totcom1dsa    2     88
293500030528     o                       totdif1sa     j    104
293600030528     o                       totper1sa     j    116
293700030528     o
293800060913     o                       totstm1psa    j    128
293900030528     o                       totcos1psa    2    142
294000060913     o                       totsco1psa    j    156
294100030528     o                       totcom1psa    2    170
294200030528     o                       totdif1psa    j    186
294300030528     o                       totper1psa    j    198
294400030528     O          e            detta       1
294500030528     o                                           30 'Tot.'
294600060913     o                       stime1        j     46
294700030528     o                       reali1        2     60
294800060913     o                       costi1        j     74
294900030528     o                       compe1        2     88
295000030528     o                       diffe1        j    104
295100030528     o                       tpcompe1      j    116
295200030528     o
295300060913     o                       stime1p       j    128
295400030528     o                       reali1p       2    142
295500060913     o                       costi1p       j    156
295600030528     o                       compe1p       2    170
295700030528     o                       diffe1p       j    186
295800030528     o                       tpcompe1p     j    198
295900030528     O          e            detta       0
296000030528     o                                           30 'Tot.'
296100060913     o                       stime1        j     46
296200030528     o                       reali1        2     60
296300060913     o                       costi1        j     74
296400030528     o                       compe1        2     88
296500030528     o                       diffe1        j    104
296600030528     o                       tpcompe1      j    116
296700030528     o
296800060913     o                       stime1p       j    128
296900030528     o                       reali1p       2    142
297000060913     o                       costi1p       j    156
297100030528     o                       compe1p       2    170
297200030528     o                       diffe1p       j    186
297300030528     o                       tpcompe1p     j    198
297400030528     O          e            detta       0
297500030528     o                                           30 'Tot.'
297600060913     o                       stime1        j     46
297700030528     o                       reali1        2     60
297800060913     o                       costi1        j     74
297900030528     o                       compe1        2     88
298000030528     o                       diffe1        j    104
298100030528     o                       tpcompe1      j    116
298200030528     o
298300060913     o                       stime1p       j    128
298400030528     o                       reali1p       2    142
298500060913     o                       costi1p       j    156
298600030528     o                       compe1p       2    170
298700030528     o                       diffe1p       j    186
298800030528     o                       tpcompe1p     j    198
298900030528     O          e            area        1
299000020619     O                                           12 'Totale area'
299100020619     o                       exarea              16
299200030528     o                                           30 'Aut.'
299300060913     o                       totstm2ds     j     46
299400030528     o                       totcos2ds     2     60
299500060913     o                       totsco2s      j     74
299600030528     o                       totcom2ds     2     88
299700030528     o                       totdif2s      j    104
299800030528     o                       totper2s      j    116
299900030528     o
300000060913     o                       totstm2ps     j    128
300100030528     o                       totcos2ps     2    142
300200060913     o                       totsco2ps     j    156
300300030528     o                       totcom2ps     2    170
300400030528     o                       totdif2ps     j    186
300500030528     o                       totper2ps     j    198
300600030527     O          e            area        1
300700030528     o                       exdesa              20
300800030528     o                                           30 'Coop'
300900060913     o                       totstm2dsx    j     46
301000030528     o                       totcos2dsx    2     60
301100060913     o                       totsco2sx     j     74
301200030528     o                       totcom2dsx    2     88
301300030528     o                       totdif2sx     j    104
301400030528     o                       totper2sx     j    116
301500030528     o
301600060913     o                       totstm2psx    j    128
301700030528     o                       totcos2psx    2    142
301800060913     o                       totsco2psx    j    156
301900030528     o                       totcom2psx    2    170
302000030528     o                       totdif2psx    j    186
302100030528     o                       totper2psx    j    198
302200030528     O          e            area        1
302300030528     o                                           30 'AfDe'
302400060913     o                       totstm2dsa    j     46
302500030528     o                       totcos2dsa    2     60
302600060913     o                       totsco2sa     j     74
302700030528     o                       totcom2dsa    2     88
302800030528     o                       totdif2sa     j    104
302900030528     o                       totper2sa     j    116
303000030528     o
303100060913     o                       totstm2psa    j    128
303200030528     o                       totcos2psa    2    142
303300060913     o                       totsco2psa    j    156
303400030528     o                       totcom2psa    2    170
303500030528     o                       totdif2psa    j    186
303600030528     o                       totper2psa    j    198
303700030528     O          e            area        1
303800030528     o                                           30 'Tot.'
303900060913     o                       stime2        j     46
304000030528     o                       reali2        2     60
304100060913     o                       costi2        j     74
304200030528     o                       compe2        2     88
304300030528     o                       diffe2        j    104
304400030528     o                       tpcompe2      j    116
304500030528     o
304600060913     o                       stime2p       j    128
304700030528     o                       reali2p       2    142
304800060913     o                       costi2p       j    156
304900030528     o                       compe2p       2    170
305000030528     o                       diffe2p       j    186
305100030528     o                       tpcompe2p     j    198
305200030528     O          e            area        0
305300030528     o                                           30 'Tot.'
305400060913     o                       stime2        j     46
305500030528     o                       reali2        2     60
305600060913     o                       costi2        j     74
305700030528     o                       compe2        2     88
305800030528     o                       diffe2        j    104
305900030528     o                       tpcompe2      j    116
306000030528     o
306100060913     o                       stime2p       j    128
306200030528     o                       reali2p       2    142
306300060913     o                       costi2p       j    156
306400030528     o                       compe2p       2    170
306500030528     o                       diffe2p       j    186
306600030528     o                       tpcompe2p     j    198
306700030528     O          e            area        0  1
306800030528     o                                           30 'Tot.'
306900060913     o                       stime2        j     46
307000030528     o                       reali2        2     60
307100060913     o                       costi2        j     74
307200030528     o                       compe2        2     88
307300030528     o                       diffe2        j    104
307400030528     o                       tpcompe2      j    116
307500030528     o
307600060913     o                       stime2p       j    128
307700030528     o                       reali2p       2    142
307800060913     o                       costi2p       j    156
307900030528     o                       compe2p       2    170
308000030528     o                       diffe2p       j    186
308100030528     o                       tpcompe2p     j    198
308200020619     Oecoprt3   e            TESTA3            2
308300020625     o                                            8 'FICN34R'
308400020625     o                       *date         y   +  3
308500030227     o                       wora              +  2 '  :  :  '
308600030319     O                                          100 'Stampa C.E. giornaliero -
308700030314     O                                              Costi variabili'
308800030319     o                                          110 'Dal '
308900020619     o                       dalladatg         +  2 '  /  /    '
309000020620     o                                         +  2 'Al '
309100020619     o                       alladatg          +  2 '  /  /    '
309200100204     o                                         +  2 'Per Dir.Comm.'
309300020625     O          e            TESTA3      0
309400030319     O                                          100 'Stampa C.E. giornaliero -
309500030314     O                                              Costi variabili'
309600030319     o                                          110 'Dal '
309700020625     o                       dalladatg         +  2 '  /  /    '
309800020625     o                                         +  2 'Al '
309900020625     o                       alladatg          +  2 '  /  /    '
310000100204     o                                         +  2 'Per Dir.Comm.'
310100020625     O          e            TESTA3      0
310200030319     O                                          100 'Stampa C.E. giornaliero -
310300030314     O                                              Costi variabili'
310400030319     o                                          110 'Dal '
310500020625     o                       dalladatg         +  2 '  /  /    '
310600020625     o                                         +  2 'Al '
310700020625     o                       alladatg          +  2 '  /  /    '
310800100204     o                                         +  2 'Per Dir.Comm.'
310900020619     O          e            testa3      2
311000020620     O                                           12 'P.Operativo'
311100020620     O                                           80 'Giornata del'
311200020620     o                       alladatg            95 '  /  /    '
311300020620     O                                          150 'Progressivo al'
311400020620     o                       alladatg           165 '  /  /    '
311500020625     O          e            testa3      0
311600020625     O                                           12 'P.Operativo'
311700020625     O                                           80 'Giornata del'
311800020625     o                       alladatg            95 '  /  /    '
311900020625     O                                          150 'Progressivo al'
312000020625     o                       alladatg           165 '  /  /    '
312100020625     O          e            testa3      0
312200020625     O                                           12 'P.Operativo'
312300020625     O                                           80 'Giornata del'
312400020625     o                       alladatg            95 '  /  /    '
312500020625     O                                          150 'Progressivo al'
312600020625     o                       alladatg           165 '  /  /    '
312700020619     O          e            testa3      1
312800060913     o                                           47 'C.Stimati'
312900030528     o                                           60 'Costi'
313000030528     o                                           74 'Costi'
313100030528     o                                           88 'Competenze'
313200030528     o                                          110 'Scostamento'
313300030528      *
313400060913     o                                          130 'C.Stimati'
313500030528     o                                          142 'Costi'
313600030528     o                                          156 'Costi'
313700030528     o                                          170 'Competenze'
313800030528     o                                          192 'Scostamento'
313900020625     O          e            testa3      0
314000060913     o                                           47 'C.Stimati'
314100030528     o                                           60 'Costi'
314200030528     o                                           74 'Costi'
314300030528     o                                           88 'Competenze'
314400030528     o                                          110 'Scostamento'
314500030528      *
314600060913     o                                          130 'C.Stimati'
314700030528     o                                          142 'Costi'
314800030528     o                                          156 'Costi'
314900030528     o                                          170 'Competenze'
315000030528     o                                          192 'Scostamento'
315100020625     O          e            testa3      0
315200060913     o                                           47 'C.Stimati'
315300030528     o                                           60 'Costi'
315400030528     o                                           74 'Costi'
315500030528     o                                           88 'Competenze'
315600030528     o                                          110 'Scostamento'
315700030528      *
315800060913     o                                          130 'C.Stimati'
315900030528     o                                          142 'Costi'
316000030528     o                                          156 'Costi'
316100030528     o                                          170 'Competenze'
316200030528     o                                          192 'Scostamento'
316300020619     O          e            testa3      1
316400060913     o                                           48 'Rettifiche'
316500030528     o                                           60 'Reali'
316600030528     o                                           74 'Totali'
316700030528     o                                          104 'Valore'
316800030528     o                                          116 'Delta %'
316900030528      *
317000060913     o                                          130 'Rettifiche'
317100030528     o                                          142 'Reali'
317200030528     o                                          156 'Totali'
317300030528     o                                          186 'Valore'
317400030528     o                                          198 'Delta %'
317500020625     O          e            testa3      0
317600060913     o                                           48 'Rettifiche'
317700030528     o                                           60 'Reali'
317800030528     o                                           74 'Totali'
317900030528     o                                          104 'Valore'
318000030528     o                                          116 'Delta %'
318100030528      *
318200060913     o                                          130 'Rettifiche'
318300030528     o                                          142 'Reali'
318400030528     o                                          156 'Totali'
318500030528     o                                          186 'Valore'
318600030528     o                                          198 'Delta %'
318700030528     O          e            testa3      0
318800060913     o                                           48 'Rettifiche'
318900030528     o                                           60 'Reali'
319000030528     o                                           74 'Totali'
319100030528     o                                          104 'Valore'
319200030528     o                                          116 'Delta %'
319300030528      *
319400060913     o                                          130 'Rettifiche'
319500030528     o                                          142 'Reali'
319600030528     o                                          156 'Totali'
319700030528     o                                          186 'Valore'
319800030528     o                                          198 'Delta %'
319900020619     O          e            detta       1
320000020620     O                       exfil                3
320100020620     o                       desfil              20
320200030527     o                                           30 'Aut.'
320300060913     o                       totstm1ds     j     46
320400030528     o                       totcos1ds     2     60
320500060913     o                       totsco1s      j     74
320600030528     o                       totcom1ds     2     88
320700030528     o                       totdif1s      j    104
320800030528     o                       totper1s      j    116
320900030528     o
321000060913     o                       totstm1ps     j    128
321100030528     o                       totcos1ps     2    142
321200060913     o                       totsco1ps     j    156
321300030528     o                       totcom1ps     2    170
321400030528     o                       totdif1ps     j    186
321500030528     o                       totper1ps     j    198
321600030115     O          e            detta       1
321700030527     o                                           30 'Coop'
321800060913     o                       totstm1dsx    j     46
321900030528     o                       totcos1dsx    2     60
322000060913     o                       totsco1sx     j     74
322100030528     o                       totcom1dsx    2     88
322200030528     o                       totdif1sx     j    104
322300030528     o                       totper1sx     j    116
322400030528     o
322500060913     o                       totstm1psx    j    128
322600030528     o                       totcos1psx    2    142
322700060913     o                       totsco1psx    j    156
322800030528     o                       totcom1psx    2    170
322900030528     o                       totdif1psx    j    186
323000030528     o                       totper1psx    j    198
323100030515     O          e            detta       1
323200030527     o                                           30 'AfDe'
323300060913     o                       totstm1dsa    j     46
323400030528     o                       totcos1dsa    2     60
323500060913     o                       totsco1sa     j     74
323600030528     o                       totcom1dsa    2     88
323700030528     o                       totdif1sa     j    104
323800030528     o                       totper1sa     j    116
323900030528     o
324000060913     o                       totstm1psa    j    128
324100030528     o                       totcos1psa    2    142
324200060913     o                       totsco1psa    j    156
324300030528     o                       totcom1psa    2    170
324400030528     o                       totdif1psa    j    186
324500030528     o                       totper1psa    j    198
324600030528     O          e            detta       1
324700030528     o                                           30 'Tot.'
324800060913     o                       stime1        j     46
324900030528     o                       reali1        2     60
325000060913     o                       costi1        j     74
325100030528     o                       compe1        2     88
325200030528     o                       diffe1        j    104
325300030528     o                       tpcompe1      j    116
325400030528     o
325500060913     o                       stime1p       j    128
325600030528     o                       reali1p       2    142
325700060913     o                       costi1p       j    156
325800030528     o                       compe1p       2    170
325900030528     o                       diffe1p       j    186
326000030528     o                       tpcompe1p     j    198
326100030528     O          e            detta       0
326200030528     o                                           30 'Tot.'
326300060913     o                       stime1        j     46
326400030528     o                       reali1        2     60
326500060913     o                       costi1        j     74
326600030528     o                       compe1        2     88
326700030528     o                       diffe1        j    104
326800030528     o                       tpcompe1      j    116
326900030528     o
327000060913     o                       stime1p       j    128
327100030528     o                       reali1p       2    142
327200060913     o                       costi1p       j    156
327300030528     o                       compe1p       2    170
327400030528     o                       diffe1p       j    186
327500030528     o                       tpcompe1p     j    198
327600030528     O          e            detta       0
327700030528     o                                           30 'Tot.'
327800060913     o                       stime1        j     46
327900030528     o                       reali1        2     60
328000060913     o                       costi1        j     74
328100030528     o                       compe1        2     88
328200030528     o                       diffe1        j    104
328300030528     o                       tpcompe1      j    116
328400030528     o
328500060913     o                       stime1p       j    128
328600030528     o                       reali1p       2    142
328700060913     o                       costi1p       j    156
328800030528     o                       compe1p       2    170
328900030528     o                       diffe1p       j    186
329000030528     o                       tpcompe1p     j    198
329100030528     O          e            area        1
329200020619     O                                           12 'Totale area'
329300020619     o                       exarea              16
329400030528     o                                           30 'Aut.'
329500060913     o                       totstm2ds     j     46
329600030528     o                       totcos2ds     2     60
329700060913     o                       totsco2s      j     74
329800030528     o                       totcom2ds     2     88
329900030528     o                       totdif2s      j    104
330000030528     o                       totper2s      j    116
330100030528     o
330200060913     o                       totstm2ps     j    128
330300030528     o                       totcos2ps     2    142
330400060913     o                       totsco2ps     j    156
330500030528     o                       totcom2ps     2    170
330600030528     o                       totdif2ps     j    186
330700030528     o                       totper2ps     j    198
330800030527     O          e            area        1
330900030528     o                       exdesa              20
331000030528     o                                           30 'Coop'
331100060913     o                       totstm2dsx    j     46
331200030528     o                       totcos2dsx    2     60
331300060913     o                       totsco2sx     j     74
331400030528     o                       totcom2dsx    2     88
331500030528     o                       totdif2sx     j    104
331600030528     o                       totper2sx     j    116
331700030528     o
331800060913     o                       totstm2psx    j    128
331900030528     o                       totcos2psx    2    142
332000060913     o                       totsco2psx    j    156
332100030528     o                       totcom2psx    2    170
332200030528     o                       totdif2psx    j    186
332300030528     o                       totper2psx    j    198
332400030528     O          e            area        1
332500030528     o                                           30 'AfDe'
332600060913     o                       totstm2dsa    j     46
332700030528     o                       totcos2dsa    2     60
332800060913     o                       totsco2sa     j     74
332900030528     o                       totcom2dsa    2     88
333000030528     o                       totdif2sa     j    104
333100030528     o                       totper2sa     j    116
333200030528     o
333300060913     o                       totstm2psa    j    128
333400030528     o                       totcos2psa    2    142
333500060913     o                       totsco2psa    j    156
333600030528     o                       totcom2psa    2    170
333700030528     o                       totdif2psa    j    186
333800030528     o                       totper2psa    j    198
333900030528     O          e            area        1
334000030528     o                                           30 'Tot.'
334100060913     o                       stime2        j     46
334200030528     o                       reali2        2     60
334300060913     o                       costi2        j     74
334400030528     o                       compe2        2     88
334500030528     o                       diffe2        j    104
334600030528     o                       tpcompe2      j    116
334700030528     o
334800060913     o                       stime2p       j    128
334900030528     o                       reali2p       2    142
335000060913     o                       costi2p       j    156
335100030528     o                       compe2p       2    170
335200030528     o                       diffe2p       j    186
335300030528     o                       tpcompe2p     j    198
335400030528     O          e            area        0
335500030528     o                                           30 'Tot.'
335600060913     o                       stime2        j     46
335700030528     o                       reali2        2     60
335800060913     o                       costi2        j     74
335900030528     o                       compe2        2     88
336000030528     o                       diffe2        j    104
336100030528     o                       tpcompe2      j    116
336200030528     o
336300060913     o                       stime2p       j    128
336400030528     o                       reali2p       2    142
336500060913     o                       costi2p       j    156
336600030528     o                       compe2p       2    170
336700030528     o                       diffe2p       j    186
336800030528     o                       tpcompe2p     j    198
336900030528     O          e            area        0  1
337000030528     o                                           30 'Tot.'
337100060913     o                       stime2        j     46
337200030528     o                       reali2        2     60
337300060913     o                       costi2        j     74
337400030528     o                       compe2        2     88
337500030528     o                       diffe2        j    104
337600030528     o                       tpcompe2      j    116
337700030528     o
337800060913     o                       stime2p       j    128
337900030528     o                       reali2p       2    142
338000060913     o                       costi2p       j    156
338100030528     o                       compe2p       2    170
338200030528     o                       diffe2p       j    186
338300030528     o                       tpcompe2p     j    198
338400030528     O          e            distret     1
338500100204     O                                           18 'Totale Dir.Comm.'
338600020620     o                       exdistr             20
338700030528     o                                           30 'Aut.'
338800060913     o                       totstm3ds     j     46
338900030528     o                       totcos3ds     2     60
339000060913     o                       totsco3s      j     74
339100030528     o                       totcom3ds     2     88
339200030528     o                       totdif3s      j    104
339300030528     o                       totper3s      j    116
339400030528     o
339500060913     o                       totstm3ps     j    128
339600030528     o                       totcos3ps     2    142
339700060913     o                       totsco3ps     j    156
339800030528     o                       totcom3ps     2    170
339900030528     o                       totdif3ps     j    186
340000030528     o                       totper3ps     j    198
340100030527     O          e            distret     1
340200030528     o                       exdesd              20
340300030528     o                                           30 'Coop'
340400060913     o                       totstm3dsx    j     46
340500030528     o                       totcos3dsx    2     60
340600060913     o                       totsco3sx     j     74
340700030528     o                       totcom3dsx    2     88
340800030528     o                       totdif3sx     j    104
340900030528     o                       totper3sx     j    116
341000030528     o
341100060913     o                       totstm3psx    j    128
341200030528     o                       totcos3psx    2    142
341300060913     o                       totsco3psx    j    156
341400030528     o                       totcom3psx    2    170
341500030528     o                       totdif3psx    j    186
341600030528     o                       totper3psx    j    198
341700030528     O          e            distret     1
341800030528     o                                           30 'AfDe'
341900060913     o                       totstm3dsa    j     46
342000030528     o                       totcos3dsa    2     60
342100060913     o                       totsco3sa     j     74
342200030528     o                       totcom3dsa    2     88
342300030528     o                       totdif3sa     j    104
342400030528     o                       totper3sa     j    116
342500030528     o
342600060913     o                       totstm3psa    j    128
342700030528     o                       totcos3psa    2    142
342800060913     o                       totsco3psa    j    156
342900030528     o                       totcom3psa    2    170
343000030528     o                       totdif3psa    j    186
343100030528     o                       totper3psa    j    198
343200030528     O          e            distret     1
343300030528     o                                           30 'Tot.'
343400060913     o                       stime3        j     46
343500030528     o                       reali3        2     60
343600060913     o                       costi3        j     74
343700030528     o                       compe3        2     88
343800030528     o                       diffe3        j    104
343900030528     o                       tpcompe3      j    116
344000030528     o
344100060913     o                       stime3p       j    128
344200030528     o                       reali3p       2    142
344300060913     o                       costi3p       j    156
344400030528     o                       compe3p       2    170
344500030528     o                       diffe3p       j    186
344600030528     o                       tpcompe3p     j    198
344700030528     O          e            distret     0
344800030528     o                                           30 'Tot.'
344900060913     o                       stime3        j     46
345000030528     o                       reali3        2     60
345100060913     o                       costi3        j     74
345200060913     o                       compe3        2     88
345300030528     o                       diffe3        j    104
345400030528     o                       tpcompe3      j    116
345500030528     o
345600060913     o                       stime3p       j    128
345700030528     o                       reali3p       2    142
345800060913     o                       costi3p       j    156
345900030528     o                       compe3p       2    170
346000030528     o                       diffe3p       j    186
346100030528     o                       tpcompe3p     j    198
346200030528     O          e            distret     0  1
346300030528     o                                           30 'Tot.'
346400060913     o                       stime3        j     46
346500030528     o                       reali3        2     60
346600060913     o                       costi3        j     74
346700030528     o                       compe3        2     88
346800030528     o                       diffe3        j    104
346900030528     o                       tpcompe3      j    116
347000030528     o
347100060913     o                       stime3p       j    128
347200030528     o                       reali3p       2    142
347300060913     o                       costi3p       j    156
347400030528     o                       compe3p       2    170
347500030528     o                       diffe3p       j    186
347600030528     o                       tpcompe3p     j    198
347700020619     Oecoprt4   e            TESTA4            2
347800020625     o                                            8 'FICN34R'
347900020625     o                       *date         y   +  3
348000030227     o                       wora              +  2 '  :  :  '
348100030319     O                                          100 'Stampa C.E. giornaliero -
348200030314     O                                              Costi variabili'
348300030319     o                                          110 'Dal '
348400020619     o                       dalladatg         +  2 '  /  /    '
348500020620     o                                         +  2 'Al '
348600020619     o                       alladatg          +  2 '  /  /    '
348700020625     o                                         +  2 'Per Azienda'
348800030227     o                                          185 'Pag.'
348900030227     o                       page              +  1
349000020625     O          e            TESTA4      0
349100030319     O                                          100 'Stampa C.E. giornaliero -
349200030314     O                                              Costi variabili'
349300030319     o                                          110 'Dal '
349400020625     o                       dalladatg         +  2 '  /  /    '
349500020625     o                                         +  2 'Al '
349600020625     o                       alladatg          +  2 '  /  /    '
349700020625     o                                         +  2 'Per Azienda'
349800020625     O          e            TESTA4      0
349900030319     O                                          100 'Stampa C.E. giornaliero -
350000030314     O                                              Costi variabili'
350100030319     o                                          110 'Dal '
350200020625     o                       dalladatg         +  2 '  /  /    '
350300020625     o                                         +  2 'Al '
350400020625     o                       alladatg          +  2 '  /  /    '
350500020625     o                                         +  2 'Per Azienda'
350600020619     O          e            testa4      2
350700020620     O                                           12 'P.Operativo'
350800020620     O                                           80 'Giornata del'
350900020620     o                       alladatg            95 '  /  /    '
351000020620     O                                          150 'Progressivo al'
351100020620     o                       alladatg           165 '  /  /    '
351200020625     O          e            testa4      0
351300020625     O                                           12 'P.Operativo'
351400020625     O                                           80 'Giornata del'
351500020625     o                       alladatg            95 '  /  /    '
351600020625     O                                          150 'Progressivo al'
351700020625     o                       alladatg           165 '  /  /    '
351800020625     O          e            testa4      0
351900020625     O                                           12 'P.Operativo'
352000020625     O                                           80 'Giornata del'
352100020625     o                       alladatg            95 '  /  /    '
352200020625     O                                          150 'Progressivo al'
352300020625     o                       alladatg           165 '  /  /    '
352400020619     O          e            testa4      1
352500060913     o                                           47 'C.Stimati'
352600030528     o                                           60 'Costi'
352700030528     o                                           74 'Costi'
352800030528     o                                           88 'Competenze'
352900030528     o                                          110 'Scostamento'
353000030528      *
353100060913     o                                          130 'C.Stimati'
353200030528     o                                          142 'Costi'
353300030528     o                                          156 'Costi'
353400030528     o                                          170 'Competenze'
353500030528     o                                          192 'Scostamento'
353600020625     O          e            testa4      0
353700060913     o                                           47 'C.Stimati'
353800030528     o                                           60 'Costi'
353900030528     o                                           74 'Costi'
354000030528     o                                           88 'Competenze'
354100030528     o                                          110 'Scostamento'
354200030528      *
354300060913     o                                          130 'C.Stimati'
354400030528     o                                          142 'Costi'
354500030528     o                                          156 'Costi'
354600030528     o                                          170 'Competenze'
354700030528     o                                          192 'Scostamento'
354800020625     O          e            testa4      0
354900060913     o                                           47 'C.Stimati'
355000030528     o                                           60 'Costi'
355100030528     o                                           74 'Costi'
355200030528     o                                           88 'Competenze'
355300030528     o                                          110 'Scostamento'
355400030528      *
355500060913     o                                          130 'C.Stimati'
355600030528     o                                          142 'Costi'
355700030528     o                                          156 'Costi'
355800030528     o                                          170 'Competenze'
355900030528     o                                          192 'Scostamento'
356000020619     O          e            testa4      1
356100060913     o                                           48 'Rettifiche'
356200030528     o                                           60 'Reali'
356300030528     o                                           74 'Totali'
356400030528     o                                          104 'Valore'
356500030528     o                                          116 'Delta %'
356600030528      *
356700060913     o                                          130 'Rettifiche'
356800030528     o                                          142 'Reali'
356900030528     o                                          156 'Totali'
357000030528     o                                          186 'Valore'
357100030528     o                                          198 'Delta %'
357200020625     O          e            testa4      0
357300060913     o                                           48 'Rettifiche'
357400030528     o                                           60 'Reali'
357500030528     o                                           74 'Totali'
357600030528     o                                          104 'Valore'
357700030528     o                                          116 'Delta %'
357800030528      *
357900060913     o                                          130 'Rettifiche'
358000030528     o                                          142 'Reali'
358100030528     o                                          156 'Totali'
358200030528     o                                          186 'Valore'
358300030528     o                                          198 'Delta %'
358400020625     O          e            testa4      0
358500060913     o                                           48 'Rettifiche'
358600030528     o                                           60 'Reali'
358700030528     o                                           74 'Totali'
358800030528     o                                          104 'Valore'
358900030528     o                                          116 'Delta %'
359000030528      *
359100060913     o                                          130 'Rettifiche'
359200030528     o                                          142 'Reali'
359300030528     o                                          156 'Totali'
359400030528     o                                          186 'Valore'
359500030528     o                                          198 'Delta %'
359600020619     O          e            detta       1
359700020620     O                       exfil                3
359800020620     o                       desfil              20
359900030527     o                                           30 'Aut.'
360000060913     o                       totstm1ds     j     46
360100030528     o                       totcos1ds     2     60
360200060913     o                       totsco1s      j     74
360300030528     o                       totcom1ds     2     88
360400030528     o                       totdif1s      j    104
360500030528     o                       totper1s      j    116
360600030528     o
360700060913     o                       totstm1ps     j    128
360800030528     o                       totcos1ps     2    142
360900060913     o                       totsco1ps     j    156
361000030528     o                       totcom1ps     2    170
361100030528     o                       totdif1ps     j    186
361200030528     o                       totper1ps     j    198
361300030115     O          e            detta       1
361400030527     o                                           30 'Coop'
361500060913     o                       totstm1dsx    j     46
361600030528     o                       totcos1dsx    2     60
361700060913     o                       totsco1sx     j     74
361800030528     o                       totcom1dsx    2     88
361900030528     o                       totdif1sx     j    104
362000030528     o                       totper1sx     j    116
362100030528     o
362200060913     o                       totstm1psx    j    128
362300030528     o                       totcos1psx    2    142
362400060913     o                       totsco1psx    j    156
362500030528     o                       totcom1psx    2    170
362600030528     o                       totdif1psx    j    186
362700030528     o                       totper1psx    j    198
362800030515     O          e            detta       1
362900030527     o                                           30 'AfDe'
363000060913     o                       totstm1dsa    j     46
363100030528     o                       totcos1dsa    2     60
363200060913     o                       totsco1sa     j     74
363300030528     o                       totcom1dsa    2     88
363400030528     o                       totdif1sa     j    104
363500030528     o                       totper1sa     j    116
363600030528     o
363700060913     o                       totstm1psa    j    128
363800030528     o                       totcos1psa    2    142
363900060913     o                       totsco1psa    j    156
364000030528     o                       totcom1psa    2    170
364100030528     o                       totdif1psa    j    186
364200030528     o                       totper1psa    j    198
364300030528     O          e            detta       1
364400030528     o                                           30 'Tot.'
364500060913     o                       stime1        j     46
364600030528     o                       reali1        2     60
364700060913     o                       costi1        j     74
364800030528     o                       compe1        2     88
364900030528     o                       diffe1        j    104
365000030528     o                       tpcompe1      j    116
365100030528     o
365200060913     o                       stime1p       j    128
365300030528     o                       reali1p       2    142
365400060913     o                       costi1p       j    156
365500030528     o                       compe1p       2    170
365600030528     o                       diffe1p       j    186
365700030528     o                       tpcompe1p     j    198
365800030528     O          e            detta       0
365900030528     o                                           30 'Tot.'
366000060913     o                       stime1        j     46
366100030528     o                       reali1        2     60
366200060913     o                       costi1        j     74
366300030528     o                       compe1        2     88
366400030528     o                       diffe1        j    104
366500030528     o                       tpcompe1      j    116
366600030528     o
366700060913     o                       stime1p       j    128
366800030528     o                       reali1p       2    142
366900060913     o                       costi1p       j    156
367000030528     o                       compe1p       2    170
367100030528     o                       diffe1p       j    186
367200030528     o                       tpcompe1p     j    198
367300030528     O          e            detta       0
367400030528     o                                           30 'Tot.'
367500060913     o                       stime1        j     46
367600030528     o                       reali1        2     60
367700060913     o                       costi1        j     74
367800030528     o                       compe1        2     88
367900030528     o                       diffe1        j    104
368000030528     o                       tpcompe1      j    116
368100030528     o
368200060913     o                       stime1p       j    128
368300030528     o                       reali1p       2    142
368400060913     o                       costi1p       j    156
368500030528     o                       compe1p       2    170
368600030528     o                       diffe1p       j    186
368700030528     o                       tpcompe1p     j    198
368800030528     O          e            area        1
368900020619     O                                           12 'Totale area'
369000020619     o                       exarea              16
369100030528     o                                           30 'Aut.'
369200060913     o                       totstm2ds     j     46
369300030528     o                       totcos2ds     2     60
369400060913     o                       totsco2s      j     74
369500030528     o                       totcom2ds     2     88
369600030528     o                       totdif2s      j    104
369700030528     o                       totper2s      j    116
369800030528     o
369900060913     o                       totstm2ps     j    128
370000030528     o                       totcos2ps     2    142
370100060913     o                       totsco2ps     j    156
370200030528     o                       totcom2ps     2    170
370300030528     o                       totdif2ps     j    186
370400030528     o                       totper2ps     j    198
370500030527     O          e            area        1
370600030528     o                       exdesa              20
370700030528     o                                           30 'Coop'
370800060913     o                       totstm2dsx    j     46
370900030528     o                       totcos2dsx    2     60
371000060913     o                       totsco2sx     j     74
371100030528     o                       totcom2dsx    2     88
371200030528     o                       totdif2sx     j    104
371300030528     o                       totper2sx     j    116
371400030528     o
371500060913     o                       totstm2psx    j    128
371600030528     o                       totcos2psx    2    142
371700060913     o                       totsco2psx    j    156
371800030528     o                       totcom2psx    2    170
371900030528     o                       totdif2psx    j    186
372000030528     o                       totper2psx    j    198
372100030528     O          e            area        1
372200030528     o                                           30 'AfDe'
372300060913     o                       totstm2dsa    j     46
372400030528     o                       totcos2dsa    2     60
372500060913     o                       totsco2sa     j     74
372600030528     o                       totcom2dsa    2     88
372700030528     o                       totdif2sa     j    104
372800030528     o                       totper2sa     j    116
372900030528     o
373000060913     o                       totstm2psa    j    128
373100030528     o                       totcos2psa    2    142
373200060913     o                       totsco2psa    j    156
373300030528     o                       totcom2psa    2    170
373400030528     o                       totdif2psa    j    186
373500030528     o                       totper2psa    j    198
373600030528     O          e            area        1
373700030528     o                                           30 'Tot.'
373800060913     o                       stime2        j     46
373900030528     o                       reali2        2     60
374000060913     o                       costi2        j     74
374100030528     o                       compe2        2     88
374200030528     o                       diffe2        j    104
374300030528     o                       tpcompe2      j    116
374400030528     o
374500060913     o                       stime2p       j    128
374600030528     o                       reali2p       2    142
374700060913     o                       costi2p       j    156
374800030528     o                       compe2p       2    170
374900030528     o                       diffe2p       j    186
375000030528     o                       tpcompe2p     j    198
375100030528     O          e            area        0
375200030528     o                                           30 'Tot.'
375300060913     o                       stime2        j     46
375400030528     o                       reali2        2     60
375500060913     o                       costi2        j     74
375600030528     o                       compe2        2     88
375700030528     o                       diffe2        j    104
375800030528     o                       tpcompe2      j    116
375900030528     o
376000060913     o                       stime2p       j    128
376100030528     o                       reali2p       2    142
376200060913     o                       costi2p       j    156
376300030528     o                       compe2p       2    170
376400030528     o                       diffe2p       j    186
376500030528     o                       tpcompe2p     j    198
376600030528     O          e            area        0  1
376700030528     o                                           30 'Tot.'
376800060913     o                       stime2        j     46
376900030528     o                       reali2        2     60
377000060913     o                       costi2        j     74
377100030528     o                       compe2        2     88
377200030528     o                       diffe2        j    104
377300030528     o                       tpcompe2      j    116
377400030528     o
377500060913     o                       stime2p       j    128
377600030528     o                       reali2p       2    142
377700060913     o                       costi2p       j    156
377800030528     o                       compe2p       2    170
377900030528     o                       diffe2p       j    186
378000030528     o                       tpcompe2p     j    198
378100030528     O          e            distret     1
378200100204     O                                           18 'Totale Dir.Comm.'
378300020620     o                       exdistr             20
378400030528     o                                           30 'Aut.'
378500060913     o                       totstm3ds     j     46
378600030528     o                       totcos3ds     2     60
378700060913     o                       totsco3s      j     74
378800030528     o                       totcom3ds     2     88
378900030528     o                       totdif3s      j    104
379000030528     o                       totper3s      j    116
379100030528     o
379200060913     o                       totstm3ps     j    128
379300030528     o                       totcos3ps     2    142
379400060913     o                       totsco3ps     j    156
379500030528     o                       totcom3ps     2    170
379600030528     o                       totdif3ps     j    186
379700030528     o                       totper3ps     j    198
379800030527     O          e            distret     1
379900030528     o                       exdesd              20
380000030528     o                                           30 'Coop'
380100060913     o                       totstm3dsx    j     46
380200030528     o                       totcos3dsx    2     60
380300060913     o                       totsco3sx     j     74
380400030528     o                       totcom3dsx    2     88
380500030528     o                       totdif3sx     j    104
380600030528     o                       totper3sx     j    116
380700030528     o
380800060913     o                       totstm3psx    j    128
380900030528     o                       totcos3psx    2    142
381000060913     o                       totsco3psx    j    156
381100030528     o                       totcom3psx    2    170
381200030528     o                       totdif3psx    j    186
381300030528     o                       totper3psx    j    198
381400030528     O          e            distret     1
381500030528     o                                           30 'AfDe'
381600060913     o                       totstm3dsa    j     46
381700030528     o                       totcos3dsa    2     60
381800060913     o                       totsco3sa     j     74
381900030528     o                       totcom3dsa    2     88
382000030528     o                       totdif3sa     j    104
382100030528     o                       totper3sa     j    116
382200030528     o
382300060913     o                       totstm3psa    j    128
382400030528     o                       totcos3psa    2    142
382500060913     o                       totsco3psa    j    156
382600030528     o                       totcom3psa    2    170
382700030528     o                       totdif3psa    j    186
382800030528     o                       totper3psa    j    198
382900030528     O          e            distret     1
383000030528     o                                           30 'Tot.'
383100060913     o                       stime3        j     46
383200030528     o                       reali3        2     60
383300060913     o                       costi3        j     74
383400030528     o                       compe3        2     88
383500030528     o                       diffe3        j    104
383600030528     o                       tpcompe3      j    116
383700030528     o
383800060913     o                       stime3p       j    128
383900030528     o                       reali3p       2    142
384000060913     o                       costi3p       j    156
384100030528     o                       compe3p       2    170
384200030528     o                       diffe3p       j    186
384300030528     o                       tpcompe3p     j    198
384400030528     O          e            distret     0
384500030528     o                                           30 'Tot.'
384600060913     o                       stime3        j     46
384700030528     o                       reali3        2     60
384800060913     o                       costi3        j     74
384900030528     o                       compe3        2     88
385000030528     o                       diffe3        j    104
385100030528     o                       tpcompe3      j    116
385200030528     o
385300060913     o                       stime3p       j    128
385400030528     o                       reali3p       2    142
385500060913     o                       costi3p       j    156
385600030528     o                       compe3p       2    170
385700030528     o                       diffe3p       j    186
385800030528     o                       tpcompe3p     j    198
385900030528     O          e            distret     0  1
386000030528     o                                           30 'Tot.'
386100060913     o                       stime3        j     46
386200030528     o                       reali3        2     60
386300060913     o                       costi3        j     74
386400030528     o                       compe3        2     88
386500030528     o                       diffe3        j    104
386600030528     o                       tpcompe3      j    116
386700030528     o
386800060913     o                       stime3p       j    128
386900030528     o                       reali3p       2    142
387000060913     o                       costi3p       j    156
387100030528     o                       compe3p       2    170
387200030528     o                       diffe3p       j    186
387300030528     o                       tpcompe3p     j    198
387400030528     O          e            azienda     1
387500020619     O                                           16 'Totale azienda'
387600030609     o                                           30 'Aut.'
387700060913     o                       totstm4ds     j     46
387800030528     o                       totcos4ds     2     60
387900060913     o                       totsco4s      j     74
388000030528     o                       totcom4ds     2     88
388100030528     o                       totdif4s      j    104
388200030528     o                       totper4s      j    116
388300030528     o
388400060913     o                       totstm4ps     j    128
388500030528     o                       totcos4ps     2    142
388600060913     o                       totsco4ps     j    156
388700030528     o                       totcom4ps     2    170
388800030528     o                       totdif4ps     j    186
388900030528     o                       totper4ps     j    198
389000030115     O          e            azienda     1
389100030609     o                                           30 'Coop'
389200060913     o                       totstm4dsx    j     46
389300030528     o                       totcos4dsx    2     60
389400060913     o                       totsco4sx     j     74
389500030528     o                       totcom4dsx    2     88
389600030528     o                       totdif4sx     j    104
389700030528     o                       totper4sx     j    116
389800030528     o
389900060913     o                       totstm4psx    j    128
390000030528     o                       totcos4psx    2    142
390100060913     o                       totsco4psx    j    156
390200030528     o                       totcom4psx    2    170
390300030528     o                       totdif4psx    j    186
390400030528     o                       totper4psx    j    198
390500030515     O          e            azienda     1
390600030609     o                                           30 'AfDe'
390700060913     o                       totstm4dsa    j     46
390800030528     o                       totcos4dsa    2     60
390900060913     o                       totsco4sa     j     74
391000030528     o                       totcom4dsa    2     88
391100030528     o                       totdif4sa     j    104
391200030528     o                       totper4sa     j    116
391300030528     o
391400060913     o                       totstm4psa    j    128
391500030528     o                       totcos4psa    2    142
391600060913     o                       totsco4psa    j    156
391700030528     o                       totcom4psa    2    170
391800030528     o                       totdif4psa    j    186
391900030528     o                       totper4psa    j    198
392000030528     O          e            azienda     1
392100030528     o                                           30 'Tot.'
392200060913     o                       stime4        j     46
392300030528     o                       reali4        2     60
392400060913     o                       costi4        j     74
392500030528     o                       compe4        2     88
392600030528     o                       diffe4        j    104
392700030528     o                       tpcompe4      j    116
392800030528     o
392900060913     o                       stime4p       j    128
393000030528     o                       reali4p       2    142
393100060913     o                       costi4p       j    156
393200030528     o                       compe4p       2    170
393300030528     o                       diffe4p       j    186
393400030528     o                       tpcompe4p     j    198
393500030528     O          e            azienda     0
393600030528     o                                           30 'Tot.'
393700060913     o                       stime4        j     46
393800030528     o                       reali4        2     60
393900060913     o                       costi4        j     74
394000030528     o                       compe4        2     88
394100030528     o                       diffe4        j    104
394200030528     o                       tpcompe4      j    116
394300030528     o
394400060913     o                       stime4p       j    128
394500030528     o                       reali4p       2    142
394600060913     o                       costi4p       j    156
394700030528     o                       compe4p       2    170
394800030528     o                       diffe4p       j    186
394900030528     o                       tpcompe4p     j    198
395000030528     O          e            azienda     0
395100030528     o                                           30 'Tot.'
395200060913     o                       stime4        j     46
395300030528     o                       reali4        2     60
395400060913     o                       costi4        j     74
395500030528     o                       compe4        2     88
395600030528     o                       diffe4        j    104
395700030528     o                       tpcompe4      j    116
395800030528     o
395900060913     o                       stime4p       j    128
396000030528     o                       reali4p       2    142
396100060913     o                       costi4p       j    156
396200030528     o                       compe4p       2    170
396300030528     o                       diffe4p       j    186
396400030528     o                       tpcompe4p     j    198
396500020612**
396600030529OVRPRTF ECOPRT1 FORMTYPE(REP_C.EC) save(*YES) OUTQ(0123456789)
396700030529OVRPRTF ECOPRT2 FORMTYPE(REP_C.EC) save(*YES) OUTQ(0123456789)
396800030529OVRPRTF ECOPRT3 FORMTYPE(REP_C.EC) save(*YES) OUTQ(0123456789)
396900050221OVRPRTF ECOPRT4 FORMTYPE(REP_C.EC) save(*YES) OUTQ(P046A135S )
397000030922CLRPFM FILE(FIFSR00F)
