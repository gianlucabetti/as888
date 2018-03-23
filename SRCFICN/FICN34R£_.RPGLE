000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300020611     fazorg02l  if   e           k disk
000400020611     ffifss01l  if   e           k disk
000500030115     ffifsc01l  if   e           k disk
000600030522     ffifsa01l  if   e           k disk
000700030522     ffifre02l  if   e           k disk
000800030922     ffifsr00f  o    e             disk    usropn
000900020619     ftabel00f  if   e           k disk
001000030225     ftntbe01l  if   e           k disk
001100020619     fecoprt1   o    f  198        printer infds(stamp1) usropn
001200020619     fecoprt2   o    f  198        printer infds(stamp2) usropn
001300020619     fecoprt3   o    f  198        printer infds(stamp3) usropn
001400030508     fecoprt4   o    f  198        printer infds(stamp4) usropn
001500020612     d
001600020612     d kpjba         e ds
001700020619     d ds17          e ds
001800020619     d ds05          e ds
001900030225     d dccg          e ds
002000020612
002100020612     d param           ds
002200020612     d  dalladat                      8s 0
002300020612     d  alladat                       8s 0
002400020625     d  dist                          1    dim(10)
002500030403     d  Dovr                          1
002600050506     d  frigen                        1
002700050506     d  fudate                        8s 0
002800051123     d  riepcdg                       1
002900020612
003000020521     d rekok           s              1
003100020611     d exdistr         s                   like(orgfl3)
003200020611     d exarea          s                   like(orgcar)
003300020611     d exfil           s                   like(orgfil)
003400030527     d exfssddc        s                   like(fssddc)
003500030515      *campi totale per stime giornaliero autotrasportatori
003600030515     d totstm1d        s                   like(totcos1d)
003700030515     d totstm2d        s                   like(totcos1d)
003800030515     d totstm3d        s                   like(totcos1d)
003900030515     d totstm4d        s                   like(totcos1d)
004000030515      *campi totale per stime periodo   autotrasportatori
004100030515     d totstm1p        s                   like(totcos1d)
004200030515     d totstm2p        s                   like(totcos1d)
004300030515     d totstm3p        s                   like(totcos1d)
004400030515     d totstm4p        s                   like(totcos1d)
004500030515      *totale per stime giornaliero stampa autotrasportatori
004600030515     d totstm1ds       s                   like(totcos1ds)
004700030515     d totstm2ds       s                   like(totcos1ds)
004800030515     d totstm3ds       s                   like(totcos1ds)
004900030515     d totstm4ds       s                   like(totcos1ds)
005000030515      *totale per stime periodo stampa   autotrasportatori
005100030515     d totstm1ps       s                   like(totcos1ds)
005200030515     d totstm2ps       s                   like(totcos1ds)
005300030515     d totstm3ps       s                   like(totcos1ds)
005400030515     d totstm4ps       s                   like(totcos1ds)
005500030515      *campi totale per stime giornaliero coop
005600030515     d totstm1dx       s                   like(totcos1d)
005700030515     d totstm2dx       s                   like(totcos1d)
005800030515     d totstm3dx       s                   like(totcos1d)
005900030515     d totstm4dx       s                   like(totcos1d)
006000030515      *campi totale per stime periodo   coop
006100030515     d totstm1px       s                   like(totcos1d)
006200030515     d totstm2px       s                   like(totcos1d)
006300030515     d totstm3px       s                   like(totcos1d)
006400030515     d totstm4px       s                   like(totcos1d)
006500030515      *totale per stime giornaliero stampa coop
006600030515     d totstm1dsx      s                   like(totcos1ds)
006700030515     d totstm2dsx      s                   like(totcos1ds)
006800030515     d totstm3dsx      s                   like(totcos1ds)
006900030515     d totstm4dsx      s                   like(totcos1ds)
007000030515      *totale per stime periodo stampa coop
007100030515     d totstm1psx      s                   like(totcos1ds)
007200030515     d totstm2psx      s                   like(totcos1ds)
007300030515     d totstm3psx      s                   like(totcos1ds)
007400030515     d totstm4psx      s                   like(totcos1ds)
007500030515      *campi totale per stime giornaliero aff/def
007600030515     d totstm1da       s                   like(totcos1d)
007700030515     d totstm2da       s                   like(totcos1d)
007800030515     d totstm3da       s                   like(totcos1d)
007900030515     d totstm4da       s                   like(totcos1d)
008000030515      *campi totale per stime periodo   aff/def
008100030515     d totstm1pa       s                   like(totcos1d)
008200030515     d totstm2pa       s                   like(totcos1d)
008300030515     d totstm3pa       s                   like(totcos1d)
008400030515     d totstm4pa       s                   like(totcos1d)
008500030515      *totale per stime giornaliero stampa aff/def
008600030515     d totstm1dsa      s                   like(totcos1ds)
008700030515     d totstm2dsa      s                   like(totcos1ds)
008800030515     d totstm3dsa      s                   like(totcos1ds)
008900030515     d totstm4dsa      s                   like(totcos1ds)
009000030515      *totale per stime periodo stampa aff/def
009100030515     d totstm1psa      s                   like(totcos1ds)
009200030515     d totstm2psa      s                   like(totcos1ds)
009300030515     d totstm3psa      s                   like(totcos1ds)
009400030515     d totstm4psa      s                   like(totcos1ds)
009500030115      *CAMPI PER TOTALIZZAZIONE COSTI autotrasportatori
009600020619      *totali costi giorno
009700020625     d totcos1d        s             11  3
009800020620     d totcos2d        s                   like(totcos1d)
009900020620     d totcos3d        s                   like(totcos1d)
010000020620     d totcos4d        s                   like(totcos1d)
010100020620      *totali competenze giorno
010200020620     d totcom1d        s                   like(totcos1d)
010300020620     d totcom2d        s                   like(totcos1d)
010400020620     d totcom3d        s                   like(totcos1d)
010500020620     d totcom4d        s                   like(totcos1d)
010600020619      *totali costi periodo
010700020620     d totcos1p        s                   like(totcos1d)
010800020620     d totcos2p        s                   like(totcos1d)
010900020620     d totcos3p        s                   like(totcos1d)
011000020620     d totcos4p        s                   like(totcos1d)
011100020620      *totali competenze periodo
011200020620     d totcom1p        s                   like(totcos1d)
011300020620     d totcom2p        s                   like(totcos1d)
011400020620     d totcom3p        s                   like(totcos1d)
011500020620     d totcom4p        s                   like(totcos1d)
011600020620      *totali costi giorno solo conse
011700020620     d totcon1d        s                   like(totcos1d)
011800020620     d totcon2d        s                   like(totcos1d)
011900020620     d totcon3d        s                   like(totcos1d)
012000020620     d totcon4d        s                   like(totcos1d)
012100020620      *totali costi giorno solo ritir
012200020620     d totrit1d        s                   like(totcos1d)
012300020620     d totrit2d        s                   like(totcos1d)
012400020620     d totrit3d        s                   like(totcos1d)
012500020620     d totrit4d        s                   like(totcos1d)
012600020620      *totali costi periodo solo cons
012700020620     d totcon1p        s                   like(totcos1d)
012800020620     d totcon2p        s                   like(totcos1d)
012900020620     d totcon3p        s                   like(totcos1d)
013000020620     d totcon4p        s                   like(totcos1d)
013100020620      *totali costi periodo solo riti
013200020620     d totrit1p        s                   like(totcos1d)
013300020620     d totrit2p        s                   like(totcos1d)
013400020620     d totrit3p        s                   like(totcos1d)
013500020620     d totrit4p        s                   like(totcos1d)
013600020619      *totali competenze giorno solo consegne
013700020620     d totcon1dc       s                   like(totcos1d)
013800020620     d totcon2dc       s                   like(totcos1d)
013900020620     d totcon3dc       s                   like(totcos1d)
014000020620     d totcon4dc       s                   like(totcos1d)
014100020620      *totali competenze giorno solo
014200020620     d totrit1dc       s                   like(totcos1d)
014300020620     d totrit2dc       s                   like(totcos1d)
014400020620     d totrit3dc       s                   like(totcos1d)
014500020620     d totrit4dc       s                   like(totcos1d)
014600020620      *totali competenze periodo solo
014700020620     d totcon1pc       s                   like(totcos1d)
014800020620     d totcon2pc       s                   like(totcos1d)
014900020620     d totcon3pc       s                   like(totcos1d)
015000020620     d totcon4pc       s                   like(totcos1d)
015100020619      *totali competenze periodo solo ritiri
015200020620     d totrit1pc       s                   like(totcos1d)
015300020620     d totrit2pc       s                   like(totcos1d)
015400020620     d totrit3pc       s                   like(totcos1d)
015500020620     d totrit4pc       s                   like(totcos1d)
015600030115      *CAMPI  DA ESPORRE IN STAMPA autotrasportatori
015700020625      *totali costi giorno
015800020625     d totcos1ds       s              8  0
015900020625     d totcos2ds       s                   like(totcos1ds)
016000020625     d totcos3ds       s                   like(totcos1ds)
016100020625     d totcos4ds       s                   like(totcos1ds)
016200020625      *totali competenze giorno
016300020625     d totcom1ds       s                   like(totcos1ds)
016400020625     d totcom2ds       s                   like(totcos1ds)
016500020625     d totcom3ds       s                   like(totcos1ds)
016600020625     d totcom4ds       s                   like(totcos1ds)
016700020625      *totali costi periodo
016800020625     d totcos1ps       s                   like(totcos1ds)
016900020625     d totcos2ps       s                   like(totcos1ds)
017000020625     d totcos3ps       s                   like(totcos1ds)
017100020625     d totcos4ps       s                   like(totcos1ds)
017200020625      *totali competenze periodo
017300020625     d totcom1ps       s                   like(totcos1ds)
017400020625     d totcom2ps       s                   like(totcos1ds)
017500020625     d totcom3ps       s                   like(totcos1ds)
017600020625     d totcom4ps       s                   like(totcos1ds)
017700020625      *totali costi giorno solo conse
017800020625     d totcon1ds       s                   like(totcos1ds)
017900020625     d totcon2ds       s                   like(totcos1ds)
018000020625     d totcon3ds       s                   like(totcos1ds)
018100020625     d totcon4ds       s                   like(totcos1ds)
018200020625      *totali costi giorno solo ritir
018300020625     d totrit1ds       s                   like(totcos1ds)
018400020625     d totrit2ds       s                   like(totcos1ds)
018500020625     d totrit3ds       s                   like(totcos1ds)
018600020625     d totrit4ds       s                   like(totcos1ds)
018700020625      *totali costi periodo solo cons
018800020625     d totcon1ps       s                   like(totcos1ds)
018900020625     d totcon2ps       s                   like(totcos1ds)
019000020625     d totcon3ps       s                   like(totcos1ds)
019100020625     d totcon4ps       s                   like(totcos1ds)
019200020625      *totali costi periodo solo riti
019300020625     d totrit1ps       s                   like(totcos1ds)
019400020625     d totrit2ps       s                   like(totcos1ds)
019500020625     d totrit3ps       s                   like(totcos1ds)
019600020625     d totrit4ps       s                   like(totcos1ds)
019700020625      *totali competenze giorno solo consegne
019800020625     d totcon1dcs      s                   like(totcos1ds)
019900020625     d totcon2dcs      s                   like(totcos1ds)
020000020625     d totcon3dcs      s                   like(totcos1ds)
020100020625     d totcon4dcs      s                   like(totcos1ds)
020200020625      *totali competenze giorno solo
020300020625     d totrit1dcs      s                   like(totcos1ds)
020400020625     d totrit2dcs      s                   like(totcos1ds)
020500020625     d totrit3dcs      s                   like(totcos1ds)
020600020625     d totrit4dcs      s                   like(totcos1ds)
020700020625      *totali competenze periodo solo
020800030114     d totcon1pcs      s                   like(totcos1ds)
020900020625     d totcon2pcs      s                   like(totcos1ds)
021000020625     d totcon3pcs      s                   like(totcos1ds)
021100020625     d totcon4pcs      s                   like(totcos1ds)
021200020625      *totali competenze periodo solo ritiri
021300020625     d totrit1pcs      s                   like(totcos1ds)
021400020625     d totrit2pcs      s                   like(totcos1ds)
021500020625     d totrit3pcs      s                   like(totcos1ds)
021600020625     d totrit4pcs      s                   like(totcos1ds)
021700020625      *delta % competenze costi giornata
021800030822     d totper18_2      s             18  2
021900030203     d totper1s        s              6  2
022000020625     d totper2s        s                   like(totper1s)
022100020625     d totper3s        s                   like(totper1s)
022200020625     d totper4s        s                   like(totper1s)
022300020625      *delta % competenze costi giornata
022400020625     d totper1ps       s                   like(totper1s)
022500020625     d totper2ps       s                   like(totper1s)
022600020625     d totper3ps       s                   like(totper1s)
022700020625     d totper4ps       s                   like(totper1s)
022800030516      *differenza competenze costi giornata autotrasportatori
022900030516     d totdif1s        s              8  0
023000030516     d totdif2s        s                   like(totdif1s)
023100030516     d totdif3s        s                   like(totdif1s)
023200030516     d totdif4s        s                   like(totdif1s)
023300030516      *differenza competenze costi giornata autotrasportatori
023400030516     d totdif1ps       s                   like(totdif1s)
023500030516     d totdif2ps       s                   like(totdif1s)
023600030516     d totdif3ps       s                   like(totdif1s)
023700030516     d totdif4ps       s                   like(totdif1s)
023800030516      *stimati e reali costi giornata autotrasportatori
023900030516     d totsco1s        s                   like(totdif1s)
024000030516     d totsco2s        s                   like(totdif1s)
024100030516     d totsco3s        s                   like(totdif1s)
024200030516     d totsco4s        s                   like(totdif1s)
024300030516      *stimati e reali costi giornata autotrasportatori
024400030516     d totsco1ps       s                   like(totdif1s)
024500030516     d totsco2ps       s                   like(totdif1s)
024600030516     d totsco3ps       s                   like(totdif1s)
024700030516     d totsco4ps       s                   like(totdif1s)
024800030115      * parziali per calcoli autotrasportatori
024900020620     d cosparz         s             15  5
025000020620     d comparz         s             15  5
025100020620     d conparz         s             15  5
025200020620     d ritparz         s             15  5
025300020620     d conparzc        s             15  5
025400020620     d ritparzc        s             15  5
025500030115      *CAMPI PER TOTALIZZAZIONE COSTI Coop
025600030115      *totali costi giorno
025700030115     d totcos1dx       s                   like(totcos1d)
025800030115     d totcos2dx       s                   like(totcos1d)
025900030115     d totcos3dx       s                   like(totcos1d)
026000030115     d totcos4dx       s                   like(totcos1d)
026100030115      *totali competenze giorno
026200030115     d totcom1dx       s                   like(totcos1d)
026300030115     d totcom2dx       s                   like(totcos1d)
026400030115     d totcom3dx       s                   like(totcos1d)
026500030115     d totcom4dx       s                   like(totcos1d)
026600030115      *totali costi periodo
026700030115     d totcos1px       s                   like(totcos1d)
026800030115     d totcos2px       s                   like(totcos1d)
026900030115     d totcos3px       s                   like(totcos1d)
027000030115     d totcos4px       s                   like(totcos1d)
027100030115      *totali competenze periodo
027200030115     d totcom1px       s                   like(totcos1d)
027300030115     d totcom2px       s                   like(totcos1d)
027400030115     d totcom3px       s                   like(totcos1d)
027500030115     d totcom4px       s                   like(totcos1d)
027600030115      *totali costi giorno solo conse
027700030115     d totcon1dx       s                   like(totcos1d)
027800030115     d totcon2dx       s                   like(totcos1d)
027900030115     d totcon3dx       s                   like(totcos1d)
028000030115     d totcon4dx       s                   like(totcos1d)
028100030115      *totali costi giorno solo ritir
028200030115     d totrit1dx       s                   like(totcos1d)
028300030115     d totrit2dx       s                   like(totcos1d)
028400030115     d totrit3dx       s                   like(totcos1d)
028500030115     d totrit4dx       s                   like(totcos1d)
028600030115      *totali costi periodo solo cons
028700030115     d totcon1px       s                   like(totcos1d)
028800030115     d totcon2px       s                   like(totcos1d)
028900030115     d totcon3px       s                   like(totcos1d)
029000030115     d totcon4px       s                   like(totcos1d)
029100030115      *totali costi periodo solo riti
029200030115     d totrit1px       s                   like(totcos1d)
029300030115     d totrit2px       s                   like(totcos1d)
029400030115     d totrit3px       s                   like(totcos1d)
029500030115     d totrit4px       s                   like(totcos1d)
029600030115      *totali competenze giorno solo consegne
029700030115     d totcon1dcx      s                   like(totcos1d)
029800030115     d totcon2dcx      s                   like(totcos1d)
029900030115     d totcon3dcx      s                   like(totcos1d)
030000030115     d totcon4dcx      s                   like(totcos1d)
030100030115      *totali competenze giorno solo
030200030115     d totrit1dcx      s                   like(totcos1d)
030300030115     d totrit2dcx      s                   like(totcos1d)
030400030115     d totrit3dcx      s                   like(totcos1d)
030500030115     d totrit4dcx      s                   like(totcos1d)
030600030115      *totali competenze periodo solo
030700030115     d totcon1pcx      s                   like(totcos1d)
030800030115     d totcon2pcx      s                   like(totcos1d)
030900030115     d totcon3pcx      s                   like(totcos1d)
031000030115     d totcon4pcx      s                   like(totcos1d)
031100030115      *totali competenze periodo solo ritiri
031200030115     d totrit1pcx      s                   like(totcos1d)
031300030115     d totrit2pcx      s                   like(totcos1d)
031400030115     d totrit3pcx      s                   like(totcos1d)
031500030115     d totrit4pcx      s                   like(totcos1d)
031600030115      *CAMPI  DA ESPORRE IN STAMPA Coop
031700030115      *totali costi giorno
031800030115     d totcos1dsx      s                   like(totcos1ds)
031900030115     d totcos2dsx      s                   like(totcos1ds)
032000030115     d totcos3dsx      s                   like(totcos1ds)
032100030115     d totcos4dsx      s                   like(totcos1ds)
032200030115      *totali competenze giorno
032300030115     d totcom1dsx      s                   like(totcos1ds)
032400030115     d totcom2dsx      s                   like(totcos1ds)
032500030115     d totcom3dsx      s                   like(totcos1ds)
032600030115     d totcom4dsx      s                   like(totcos1ds)
032700030115      *totali costi periodo
032800030115     d totcos1psx      s                   like(totcos1ds)
032900030115     d totcos2psx      s                   like(totcos1ds)
033000030115     d totcos3psx      s                   like(totcos1ds)
033100030115     d totcos4psx      s                   like(totcos1ds)
033200030115      *totali competenze periodo
033300030115     d totcom1psx      s                   like(totcos1ds)
033400030115     d totcom2psx      s                   like(totcos1ds)
033500030115     d totcom3psx      s                   like(totcos1ds)
033600030115     d totcom4psx      s                   like(totcos1ds)
033700030115      *totali costi giorno solo conse
033800030115     d totcon1dsx      s                   like(totcos1ds)
033900030115     d totcon2dsx      s                   like(totcos1ds)
034000030115     d totcon3dsx      s                   like(totcos1ds)
034100030115     d totcon4dsx      s                   like(totcos1ds)
034200030115      *totali costi giorno solo ritir
034300030115     d totrit1dsx      s                   like(totcos1ds)
034400030115     d totrit2dsx      s                   like(totcos1ds)
034500030115     d totrit3dsx      s                   like(totcos1ds)
034600030115     d totrit4dsx      s                   like(totcos1ds)
034700030115      *totali costi periodo solo cons
034800030115     d totcon1psx      s                   like(totcos1ds)
034900030115     d totcon2psx      s                   like(totcos1ds)
035000030115     d totcon3psx      s                   like(totcos1ds)
035100030115     d totcon4psx      s                   like(totcos1ds)
035200030115      *totali costi periodo solo riti
035300030115     d totrit1psx      s                   like(totcos1ds)
035400030115     d totrit2psx      s                   like(totcos1ds)
035500030115     d totrit3psx      s                   like(totcos1ds)
035600030115     d totrit4psx      s                   like(totcos1ds)
035700030115      *totali competenze giorno solo consegne
035800030115     d totcon1dcsx     s                   like(totcos1ds)
035900030115     d totcon2dcsx     s                   like(totcos1ds)
036000030115     d totcon3dcsx     s                   like(totcos1ds)
036100030115     d totcon4dcsx     s                   like(totcos1ds)
036200030115      *totali competenze giorno solo
036300030115     d totrit1dcsx     s                   like(totcos1ds)
036400030115     d totrit2dcsx     s                   like(totcos1ds)
036500030115     d totrit3dcsx     s                   like(totcos1ds)
036600030115     d totrit4dcsx     s                   like(totcos1ds)
036700030115      *totali competenze periodo solo
036800030115     d totcon1pcsx     s                   like(totcos1ds)
036900030115     d totcon2pcsx     s                   like(totcos1ds)
037000030115     d totcon3pcsx     s                   like(totcos1ds)
037100030115     d totcon4pcsx     s                   like(totcos1ds)
037200030115      *totali competenze periodo solo ritiri
037300030115     d totrit1pcsx     s                   like(totcos1ds)
037400030115     d totrit2pcsx     s                   like(totcos1ds)
037500030115     d totrit3pcsx     s                   like(totcos1ds)
037600030115     d totrit4pcsx     s                   like(totcos1ds)
037700030115      *delta % competenze costi giornata
037800030115     d totper1sx       s                   like(totper1s)
037900030115     d totper2sx       s                   like(totper1s)
038000030115     d totper3sx       s                   like(totper1s)
038100030115     d totper4sx       s                   like(totper1s)
038200030115      *delta % competenze costi giornata
038300030115     d totper1psx      s                   like(totper1s)
038400030115     d totper2psx      s                   like(totper1s)
038500030115     d totper3psx      s                   like(totper1s)
038600030115     d totper4psx      s                   like(totper1s)
038700030516      *differenza competenze costi giornata coop
038800030516     d totdif1sx       s                   like(totdif1s)
038900030516     d totdif2sx       s                   like(totdif1s)
039000030516     d totdif3sx       s                   like(totdif1s)
039100030516     d totdif4sx       s                   like(totdif1s)
039200030516      *differenza competenze costi giornata coop
039300030516     d totdif1psx      s                   like(totdif1s)
039400030516     d totdif2psx      s                   like(totdif1s)
039500030516     d totdif3psx      s                   like(totdif1s)
039600030516     d totdif4psx      s                   like(totdif1s)
039700030516      *stimati e reali costi giornata coop
039800030516     d totsco1sx       s                   like(totdif1s)
039900030516     d totsco2sx       s                   like(totdif1s)
040000030516     d totsco3sx       s                   like(totdif1s)
040100030516     d totsco4sx       s                   like(totdif1s)
040200030516      *stimati e reali costi giornata coop
040300030516     d totsco1psx      s                   like(totdif1s)
040400030516     d totsco2psx      s                   like(totdif1s)
040500030516     d totsco3psx      s                   like(totdif1s)
040600030516     d totsco4psx      s                   like(totdif1s)
040700030115      * parziali per calcoli Coop
040800030115     d cosparzx        s             15  5
040900030115     d comparzx        s             15  5
041000030115     d conparzx        s             15  5
041100030523     d ritparzx        s             15  5
041200030115     d conparzcx       s             15  5
041300030115     d ritparzcx       s             15  5
041400030115
041500030515      *CAMPI PER TOTALIZZAZIONE COSTI affluenze defluenze
041600030515      *totali costi giorno
041700030515     d totcos1da       s                   like(totcos1d)
041800030515     d totcos2da       s                   like(totcos1d)
041900030515     d totcos3da       s                   like(totcos1d)
042000030515     d totcos4da       s                   like(totcos1d)
042100030515      *totali competenze giorno
042200030515     d totcom1da       s                   like(totcos1d)
042300030515     d totcom2da       s                   like(totcos1d)
042400030515     d totcom3da       s                   like(totcos1d)
042500030515     d totcom4da       s                   like(totcos1d)
042600030515      *totali costi periodo
042700030515     d totcos1pa       s                   like(totcos1d)
042800030515     d totcos2pa       s                   like(totcos1d)
042900030515     d totcos3pa       s                   like(totcos1d)
043000030515     d totcos4pa       s                   like(totcos1d)
043100030515      *totali competenze periodo
043200030515     d totcom1pa       s                   like(totcos1d)
043300030515     d totcom2pa       s                   like(totcos1d)
043400030515     d totcom3pa       s                   like(totcos1d)
043500030515     d totcom4pa       s                   like(totcos1d)
043600030515      *totali costi giorno solo conse
043700030515     d totcon1da       s                   like(totcos1d)
043800030515     d totcon2da       s                   like(totcos1d)
043900030515     d totcon3da       s                   like(totcos1d)
044000030515     d totcon4da       s                   like(totcos1d)
044100030515      *totali costi giorno solo ritir
044200030515     d totrit1da       s                   like(totcos1d)
044300030515     d totrit2da       s                   like(totcos1d)
044400030515     d totrit3da       s                   like(totcos1d)
044500030515     d totrit4da       s                   like(totcos1d)
044600030515      *totali costi periodo solo cons
044700030515     d totcon1pa       s                   like(totcos1d)
044800030515     d totcon2pa       s                   like(totcos1d)
044900030515     d totcon3pa       s                   like(totcos1d)
045000030515     d totcon4pa       s                   like(totcos1d)
045100030515      *totali costi periodo solo riti
045200030515     d totrit1pa       s                   like(totcos1d)
045300030515     d totrit2pa       s                   like(totcos1d)
045400030515     d totrit3pa       s                   like(totcos1d)
045500030515     d totrit4pa       s                   like(totcos1d)
045600030515      *totali competenze giorno solo consegne
045700030515     d totcon1dca      s                   like(totcos1d)
045800030515     d totcon2dca      s                   like(totcos1d)
045900030515     d totcon3dca      s                   like(totcos1d)
046000030515     d totcon4dca      s                   like(totcos1d)
046100030515      *totali competenze giorno solo
046200030515     d totrit1dca      s                   like(totcos1d)
046300030515     d totrit2dca      s                   like(totcos1d)
046400030515     d totrit3dca      s                   like(totcos1d)
046500030515     d totrit4dca      s                   like(totcos1d)
046600030515      *totali competenze periodo solo
046700030515     d totcon1pca      s                   like(totcos1d)
046800030515     d totcon2pca      s                   like(totcos1d)
046900030515     d totcon3pca      s                   like(totcos1d)
047000030515     d totcon4pca      s                   like(totcos1d)
047100030515      *totali competenze periodo solo ritiri
047200030515     d totrit1pca      s                   like(totcos1d)
047300030515     d totrit2pca      s                   like(totcos1d)
047400030515     d totrit3pca      s                   like(totcos1d)
047500030515     d totrit4pca      s                   like(totcos1d)
047600030522      *CAMPI  DA ESPORRE IN STAMPA aff/def
047700030515      *totali costi giorno
047800030515     d totcos1dsa      s                   like(totcos1ds)
047900030515     d totcos2dsa      s                   like(totcos1ds)
048000030516     d totcos3dsa      s                   like(totcos1ds)
048100030515     d totcos4dsa      s                   like(totcos1ds)
048200030515      *totali competenze giorno
048300030515     d totcom1dsa      s                   like(totcos1ds)
048400030515     d totcom2dsa      s                   like(totcos1ds)
048500030515     d totcom3dsa      s                   like(totcos1ds)
048600030515     d totcom4dsa      s                   like(totcos1ds)
048700030515      *totali costi periodo
048800030515     d totcos1psa      s                   like(totcos1ds)
048900030515     d totcos2psa      s                   like(totcos1ds)
049000030515     d totcos3psa      s                   like(totcos1ds)
049100030515     d totcos4psa      s                   like(totcos1ds)
049200030515      *totali competenze periodo
049300030515     d totcom1psa      s                   like(totcos1ds)
049400030515     d totcom2psa      s                   like(totcos1ds)
049500030515     d totcom3psa      s                   like(totcos1ds)
049600030515     d totcom4psa      s                   like(totcos1ds)
049700030515      *totali costi giorno solo conse
049800030515     d totcon1dsa      s                   like(totcos1ds)
049900030515     d totcon2dsa      s                   like(totcos1ds)
050000030515     d totcon3dsa      s                   like(totcos1ds)
050100030515     d totcon4dsa      s                   like(totcos1ds)
050200030515      *totali costi giorno solo ritir
050300030515     d totrit1dsa      s                   like(totcos1ds)
050400030515     d totrit2dsa      s                   like(totcos1ds)
050500030515     d totrit3dsa      s                   like(totcos1ds)
050600030515     d totrit4dsa      s                   like(totcos1ds)
050700030515      *totali costi periodo solo cons
050800030515     d totcon1psa      s                   like(totcos1ds)
050900030515     d totcon2psa      s                   like(totcos1ds)
051000030515     d totcon3psa      s                   like(totcos1ds)
051100030515     d totcon4psa      s                   like(totcos1ds)
051200030515      *totali costi periodo solo riti
051300030515     d totrit1psa      s                   like(totcos1ds)
051400030515     d totrit2psa      s                   like(totcos1ds)
051500030515     d totrit3psa      s                   like(totcos1ds)
051600030515     d totrit4psa      s                   like(totcos1ds)
051700030515      *totali competenze giorno solo consegne
051800030515     d totcon1dcsa     s                   like(totcos1ds)
051900030515     d totcon2dcsa     s                   like(totcos1ds)
052000030515     d totcon3dcsa     s                   like(totcos1ds)
052100030515     d totcon4dcsa     s                   like(totcos1ds)
052200030515      *totali competenze giorno solo
052300030515     d totrit1dcsa     s                   like(totcos1ds)
052400030515     d totrit2dcsa     s                   like(totcos1ds)
052500030515     d totrit3dcsa     s                   like(totcos1ds)
052600030515     d totrit4dcsa     s                   like(totcos1ds)
052700030515      *totali competenze periodo solo
052800030515     d totcon1pcsa     s                   like(totcos1ds)
052900030515     d totcon2pcsa     s                   like(totcos1ds)
053000030515     d totcon3pcsa     s                   like(totcos1ds)
053100030515     d totcon4pcsa     s                   like(totcos1ds)
053200030515      *totali competenze periodo solo ritiri
053300030515     d totrit1pcsa     s                   like(totcos1ds)
053400030515     d totrit2pcsa     s                   like(totcos1ds)
053500030515     d totrit3pcsa     s                   like(totcos1ds)
053600030515     d totrit4pcsa     s                   like(totcos1ds)
053700030515      *delta % competenze costi giornata
053800030515     d totper1sa       s                   like(totper1s)
053900030515     d totper2sa       s                   like(totper1s)
054000030515     d totper3sa       s                   like(totper1s)
054100030515     d totper4sa       s                   like(totper1s)
054200030515      *delta % competenze costi giornata
054300030515     d totper1psa      s                   like(totper1s)
054400030515     d totper2psa      s                   like(totper1s)
054500030515     d totper3psa      s                   like(totper1s)
054600030515     d totper4psa      s                   like(totper1s)
054700030516      *differenza competenze costi giornata aff/def
054800030516     d totdif1sa       s                   like(totdif1s)
054900030516     d totdif2sa       s                   like(totdif1s)
055000030516     d totdif3sa       s                   like(totdif1s)
055100030516     d totdif4sa       s                   like(totdif1s)
055200030516      *differenza competenze costi giornata aff/def
055300030516     d totdif1psa      s                   like(totdif1s)
055400030516     d totdif2psa      s                   like(totdif1s)
055500030516     d totdif3psa      s                   like(totdif1s)
055600030516     d totdif4psa      s                   like(totdif1s)
055700030516      *stimati e reali costi giornata aff/def
055800030516     d totsco1sa       s                   like(totdif1s)
055900030516     d totsco2sa       s                   like(totdif1s)
056000030516     d totsco3sa       s                   like(totdif1s)
056100030516     d totsco4sa       s                   like(totdif1s)
056200030516      *stimati e reali costi giornata aff/def
056300030516     d totsco1psa      s                   like(totdif1s)
056400030516     d totsco2psa      s                   like(totdif1s)
056500030516     d totsco3psa      s                   like(totdif1s)
056600030516     d totsco4psa      s                   like(totdif1s)
056700030522      * parziali per calcoli aff/def
056800030515     d cosparza        s             15  5
056900030515     d comparza        s             15  5
057000030515     d conparza        s             15  5
057100030515     d ritparza        s             15  5
057200030515     d conparzca       s             15  5
057300030515     d ritparzca       s             15  5
057400030528      * campi per totali in stampa
057500030528     d stime1          s              8  0
057600030528     d stime2          s                   like(stime1)
057700030528     d stime3          s                   like(stime1)
057800030528     d stime4          s                   like(stime1)
057900030528     d stime1p         s                   like(stime1)
058000030528     d stime2p         s                   like(stime1)
058100030528     d stime3p         s                   like(stime1)
058200030528     d stime4p         s                   like(stime1)
058300030528     d reali1          s                   like(stime1)
058400030528     d reali2          s                   like(stime1)
058500030528     d reali3          s                   like(stime1)
058600030528     d reali4          s                   like(stime1)
058700030528     d reali1p         s                   like(stime1)
058800030528     d reali2p         s                   like(stime1)
058900030528     d reali3p         s                   like(stime1)
059000030528     d reali4p         s                   like(stime1)
059100030528     d costi1          s                   like(stime1)
059200030528     d costi2          s                   like(stime1)
059300030528     d costi3          s                   like(stime1)
059400030528     d costi4          s                   like(stime1)
059500030528     d costi1p         s                   like(stime1)
059600030528     d costi2p         s                   like(stime1)
059700030528     d costi3p         s                   like(stime1)
059800030528     d costi4p         s                   like(stime1)
059900030528     d compe1          s                   like(stime1)
060000030528     d compe2          s                   like(stime1)
060100030528     d compe3          s                   like(stime1)
060200030528     d compe4          s                   like(stime1)
060300030528     d compe1p         s                   like(stime1)
060400030528     d compe2p         s                   like(stime1)
060500030528     d compe3p         s                   like(stime1)
060600030528     d compe4p         s                   like(stime1)
060700030528     d diffe1          s                   like(stime1)
060800030528     d diffe2          s                   like(stime1)
060900030528     d diffe3          s                   like(stime1)
061000030528     d diffe4          s                   like(stime1)
061100030528     d diffe1p         s                   like(stime1)
061200030528     d diffe2p         s                   like(stime1)
061300030528     d diffe3p         s                   like(stime1)
061400030528     d diffe4p         s                   like(stime1)
061500030528     d tpcompe1        s                   like(totper1s)
061600030528     d tpcompe2        s                   like(totper1s)
061700030528     d tpcompe3        s                   like(totper1s)
061800030528     d tpcompe4        s                   like(totper1s)
061900030528     d tpcompe1p       s                   like(totper1s)
062000030528     d tpcompe2p       s                   like(totper1s)
062100030528     d tpcompe3p       s                   like(totper1s)
062200030528     d tpcompe4p       s                   like(totper1s)
062300030515      * parziale per calcolo stime
062400030515     d importo         s             15  5
062500030515
062600030225     d dataiso         s               d   datfmt(*iso)
062700030225     d dataiso2        s               d   datfmt(*iso)
062800030225     d dataiso3        s               d   datfmt(*iso)
062900020619     d dataeur         s               d   datfmt(*eur)
063000030225     d dalladatg       s                   like(dalladat)
063100030225     d alladatg        s                   like(alladat)
063200030225     d alladatx        s                   like(alladat)
063300030115
063400020619     d lvl             s              1  0
063500020619     d livello         s              1  0
063600020619     d PrmOutq         s             10
063700020620     d desfil          s             15
063800030922     d schedulato      s              1
063900020619      *
064000030225     D WLBDAT          DS
064100030225     D  G02DAT                 1      8  0
064200030225     D  G02INV                 9     16  0
064300030225     D  G02ERR                17     17
064400030225     D  G02TGI                18     22  0
064500030225
064600030225     d Wdata8          DS
064700030225     d  dadata                 1      8  0
064800030225     d  adata                  9     16  0
064900030225     d  GioLav                17     21  0
065000030225      *
065100020611     d stamp1          ds
065200020611     d  lin1                 367    368b 0
065300020611     d stamp2          ds
065400020611     d  lin2                 367    368b 0
065500020611     d stamp3          ds
065600020611     d  lin3                 367    368b 0
065700020614     d stamp4          ds
065800020614     d  lin4                 367    368b 0
065900020619      *
066000030922     d CMD             S             62    DIM(5) CTDATA PERRCD(1)              OVRPRTF PRTF
066100030403      *
066200030403     D OutqSede        C                   CONST('QPRINTS   ')
066300000607      *--------------------------------------------------------------------
066400000607     c
066500020611      *loop sui record di azorg
066600020611     c     *loval        setll     azorg02l
066700000607     c                   do        *hival
066800020611     c                   read      azorg02l                               99
066900020517     c   99              leave
067000031001     c*                  if        orgfag <> 'F' and orgfag <> 'A'
067100031001     c                   if        orgfag <> 'F'
067200020620     c                   iter
067300020620     c                   end
067400030307     c                   movel     orgde8        aster             1
067500030307     c                   if        aster = *blank
067600030307     c                   iter
067700030307     c                   end
067800030307     c
067900030226      * se schedulato (senza parametri in kpjbu) non controllo il distretto
068000030226      * ed elaboro tutto
068100030226     c                   if        dista <> *blank
068200020625     c     orgfl3        lookup    dist                                   89
068300020625     c  n89              iter
068400030226     c                   endif
068500020611
068600020620     c                   exsr      main
068700020620
068800020611     c                   exsr      elabora
068900020619     c                   enddo
069000020620
069100020620     c                   exsr      main
069200020620
069300030114      * stampa totali per l'azienda
069400020619     c                   if        lin4 > 60
069500020619     c                   except    testa4
069600020619     c                   end
069700030115      * autotrasportatori
069800020625     c                   eval      totcos4ds =   totcos4d
069900020625     c                   eval      totcom4ds =   totcom4d
070000020625     c                   eval      totcos4ps =   totcos4p
070100020625     c                   eval      totcom4ps =   totcom4p
070200020625     c                   eval      totcon4ds =   totcon4d
070300020625     c                   eval      totrit4ds =   totrit4d
070400020625     c                   eval      totcon4ps =   totcon4p
070500020625     c                   eval      totrit4ps =   totrit4p
070600020625     c                   eval      totcon4dcs =  totcon4dc
070700020625     c                   eval      totrit4dcs =  totrit4dc
070800020625     c                   eval      totcon4pcs =  totcon4pc
070900020625     c                   eval      totrit4pcs =  totrit4pc
071000030522
071100030522     c                   eval      totstm4ps =  totstm4p
071200030522     c                   eval      totstm4ds =  totstm4d
071300030522     c                   eval      totsco4s = totstm4d + totcos4d
071400030522     c                   eval      totsco4ps = totstm4p + totcos4p
071500030522     c                   eval      totdif4s = totcom4d - totsco4s
071600030522     c                   eval      totdif4ps = totcom4p - totsco4ps
071700020625
071800030203     c                   if        totcom4d > 0
071900030822     c                   eval      totper18_2 = ((totcom4d - totsco4s) * 100)
072000020625     c                                        / totcom4d
072100030822     c                   Z-ADD     totper18_2    totper4s
072200030822      *
072300030203     c                   setoff                                       7071
072400030203     c     totper4s      comp      999                                70
072500030203     c  n70totper4s      comp      -999                                 71
072600030203     c   70              z-add     999           totper4s
072700030203     c   71              z-add     -999          totper4s
072800030115     c                   else
072900030115     c                   z-add     0             totper4s
073000020625     c                   endif
073100020625
073200030203     c                   if        totcom4p > 0
073300030822     c                   eval      totper18_2 = ((totcom4p - totsco4ps) * 100)
073400020625     c                                        / totcom4p
073500030822     c                   Z-ADD     totper18_2    totper4ps
073600030822      *
073700030203     c                   setoff                                       7071
073800030203     c     totper4ps     comp      999                                70
073900030203     c  n70totper4ps     comp      -999                                 71
074000030203     c   70              z-add     999           totper4ps
074100030203     c   71              z-add     -999          totper4ps
074200030115     c                   else
074300030115     c                   z-add     0             totper4ps
074400020625     c                   endif
074500030115      * Coop
074600030116     c                   eval      totcos4dsx =   totcos4dx
074700030116     c                   eval      totcom4dsx =   totcom4dx
074800030116     c                   eval      totcos4psx =   totcos4px
074900030116     c                   eval      totcom4psx =   totcom4px
075000030116     c                   eval      totcon4dsx =   totcon4dx
075100030116     c                   eval      totrit4dsx =   totrit4dx
075200030116     c                   eval      totcon4psx =   totcon4px
075300030116     c                   eval      totrit4psx =   totrit4px
075400030116     c                   eval      totcon4dcsx =  totcon4dcx
075500030116     c                   eval      totrit4dcsx =  totrit4dcx
075600030116     c                   eval      totcon4pcsx =  totcon4pcx
075700030116     c                   eval      totrit4pcsx =  totrit4pcx
075800030522
075900030522     c                   eval      totstm4psx =  totstm4px
076000030522     c                   eval      totstm4dsx =  totstm4dx
076100030522     c                   eval      totsco4sx = totstm4dx + totcos4dx
076200030522     c                   eval      totsco4psx = totstm4px + totcos4px
076300030522     c                   eval      totdif4sx = totcom4dx - totsco4sx
076400030522     c                   eval      totdif4psx = totcom4px - totsco4psx
076500030522
076600030203     c                   if        totcom4dx > 0
076700030822     c                   eval      totper18_2= ((totcom4dsx - totsco4sx) * 100)
076800030609     c                                        / totcom4dsx
076900030822     c                   Z-ADD     totper18_2    totper4sx
077000030822      *
077100030203     c                   setoff                                       7071
077200030203     c     totper4sx     comp      999                                70
077300030203     c  n70totper4sx     comp      -999                                 71
077400030203     c   70              z-add     999           totper4sx
077500030203     c   71              z-add     -999          totper4sx
077600030116     c                   else
077700030116     c                   z-add     0             totper4sx
077800030116     c                   endif
077900030116
078000030203     c                   if        totcom4px > 0
078100030822     c                   eval      totper18_2 = ((totcom4px - totsco4psx) * 100)
078200030116     c                                        / totcom4px
078300030822     c                   Z-ADD     totper18_2    totper4psx
078400030822      *
078500030203     c                   setoff                                       7071
078600030203     c     totper4psx    comp      999                                70
078700030203     c  n70totper4psx    comp      -999                                 71
078800030203     c   70              z-add     999           totper4psx
078900030203     c   71              z-add     -999          totper4psx
079000030116     c                   else
079100030116     c                   z-add     0             totper4psx
079200030116     c                   endif
079300030116
079400030515      * Affluenze/defluenze
079500030515     c                   eval      totcos4dsa =   totcos4da
079600030515     c                   eval      totcom4dsa =   totcom4da
079700030515     c                   eval      totcos4psa =   totcos4pa
079800030515     c                   eval      totcom4psa =   totcom4pa
079900030515     c                   eval      totcon4dsa =   totcon4da
080000030515     c                   eval      totrit4dsa =   totrit4da
080100030515     c                   eval      totcon4psa =   totcon4pa
080200030515     c                   eval      totrit4psa =   totrit4pa
080300030515     c                   eval      totcon4dcsa =  totcon4dca
080400030515     c                   eval      totrit4dcsa =  totrit4dca
080500030515     c                   eval      totcon4pcsa =  totcon4pca
080600030515     c                   eval      totrit4pcsa =  totrit4pca
080700030522
080800030522     c                   eval      totstm4psa =  totstm4pa
080900030522     c                   eval      totstm4dsa =  totstm4da
081000030522     c                   eval      totsco4sa = totstm4da + totcos4da
081100030522     c                   eval      totsco4psa = totstm4pa + totcos4pa
081200030522     c                   eval      totdif4sa = totcom4da - totsco4sa
081300030522     c                   eval      totdif4psa = totcom4pa - totsco4psa
081400030515
081500030515     c                   if        totcom4da > 0
081600030822     c                   eval      totper18_2= ((totcom4dsa - totsco4sa) * 100)
081700030609     c                                        / totcom4dsa
081800030822     c                   Z-ADD     totper18_2    totper4sa
081900030822      *
082000030515     c                   setoff                                       7071
082100030515     c     totper4sa     comp      999                                70
082200030515     c  n70totper4sa     comp      -999                                 71
082300030515     c   70              z-add     999           totper4sa
082400030515     c   71              z-add     -999          totper4sa
082500030515     c                   else
082600030515     c                   z-add     0             totper4sa
082700030515     c                   endif
082800030515
082900030515     c                   if        totcom4pa > 0
083000030822     c                   eval      totper18_2 = ((totcom4pa - totsco4psa) * 100)
083100030515     c                                        / totcom4pa
083200030822     c                   Z-ADD     totper18_2    totper4psa
083300030822      *
083400030515     c                   setoff                                       7071
083500030515     c     totper4psa    comp      999                                70
083600030515     c  n70totper4psa    comp      -999                                 71
083700030515     c   70              z-add     999           totper4psa
083800030515     c   71              z-add     -999          totper4psa
083900030515     c                   else
084000030515     c                   z-add     0             totper4psa
084100030515     c                   endif
084200030528      * ricavo i totali per azienda
084300030528      *giornalieri
084400030528     c                   eval      stime4 = totstm4ds + totstm4dsx + totstm4dsa
084500030528     c                   eval      reali4 = totcos4ds + totcos4dsx + totcos4dsa
084600030528     c                   eval      costi4 = totsco4s + totsco4sx + totsco4sa
084700030528     c                   eval      compe4 = totcom4ds + totcom4dsx + totcom4dsa
084800030528     c                   eval      diffe4 = totdif4s + totdif4sx + totdif4sa
084900030528
085000030528      *periodo
085100030528     c                   eval      stime4p= totstm4ps + totstm4psx + totstm4psa
085200030528     c                   eval      reali4p= totcos4ps + totcos4psx + totcos4psa
085300030528     c                   eval      costi4p= totsco4ps + totsco4psx + totsco4psa
085400030528     c                   eval      compe4p= totcom4ps + totcom4psx + totcom4psa
085500030528     c                   eval      diffe4p= totdif4ps + totdif4psx + totdif4psa
085600030528      *calcola le percentuali
085700030528     c                   if        compe4  > 0
085800030822     c                   eval      totper18_2 = ((compe4 -costi4) * 100) /compe4
085900030822     c                   Z-ADD     totper18_2    tpcompe4
086000030822      *
086100030528     c                   setoff                                       7071
086200030528     c     tpcompe4      comp      999                                70
086300030528     c  n70tpcompe4      comp      -999                                 71
086400030528     c   70              z-add     999           tpcompe4
086500030528     c   71              z-add     -999          tpcompe4
086600030528     c                   else
086700030528     c                   z-add     0             tpcompe4
086800030528     c                   endif
086900030528
087000030528     c                   if        compe4p > 0
087100030822     c                   eval      totper18_2 = ((compe4p-costi4p) * 100)
087200030822     c                                        / compe4p
087300030822     c                   Z-ADD     totper18_2    tpcompe4p
087400030822      *
087500030528     c                   setoff                                       7071
087600030528     c     tpcompe4p     comp      999                                70
087700030528     c  n70tpcompe4p     comp      -999                                 71
087800030528     c   70              z-add     999           tpcompe4p
087900030528     c   71              z-add     -999          tpcompe4p
088000030528     c                   else
088100030528     c                   z-add     0             tpcompe4p
088200030528     c                   endif
088300030515
088400020619     c                   except    azienda
088500020611
088600020521     c                   seton                                        lr
088700020620      *-------------------------------------------------
088800020620     c     main          begsr
088900020620      *------------------------------------------------
089000100210     c                   if        rekok <> *blank
089100020620
089200020620      *rottura filiale
089300020620     c                   if        orgfil <> exfil
089400020620     c                   except    testa1
089500020620     c                   exsr      testate
089600030115      *imposto i  campi x la stampa autotrasportatori
089700020625     c                   eval      totcos1ds =   totcos1d
089800020625     c                   eval      totcom1ds =   totcom1d
089900020625     c                   eval      totcos1ps =   totcos1p
090000020625     c                   eval      totcom1ps =   totcom1p
090100020625     c                   eval      totcon1ds =   totcon1d
090200020625     c                   eval      totrit1ds =   totrit1d
090300020625     c                   eval      totcon1ps =   totcon1p
090400020625     c                   eval      totrit1ps =   totrit1p
090500020625     c                   eval      totcon1dcs =  totcon1dc
090600020625     c                   eval      totrit1dcs =  totrit1dc
090700030114     c                   eval      totcon1pcs =  totcon1pc
090800020625     c                   eval      totrit1pcs =  totrit1pc
090900020625
091000030522     c                   eval      totstm1ps  =  totstm1p
091100030522     c                   eval      totstm1ds  =  totstm1d
091200030522     c                   eval      totsco1s  = totstm1d  + totcos1d
091300030522     c                   eval      totsco1ps  = totstm1p  + totcos1p
091400030522     c                   eval      totdif1s  = totcom1d  - totsco1s
091500030522     c                   eval      totdif1ps  = totcom1p  - totsco1ps
091600030522
091700030203     c                   if        totcom1d > 0
091800030822     c                   eval      totper18_2 = ((totcom1d - totsco1s) * 100)
091900020625     c                                        / totcom1d
092000030822     c                   Z-ADD     totper18_2    totper1s
092100030822      *
092200030203     c                   setoff                                       7071
092300030203     c     totper1s      comp      999                                70
092400030203     c  n70totper1s      comp      -999                                 71
092500030203     c   70              z-add     999           totper1s
092600030203     c   71              z-add     -999          totper1s
092700030116     c                   else
092800030116     c                   z-add     0             totper1s
092900020625     c                   endif
093000020625
093100030203     c                   if        totcom1p > 0
093200030822     c                   eval      totper18_2= ((totcom1p - totsco1ps) * 100)
093300020625     c                                        / totcom1p
093400030822     c                   Z-ADD     totper18_2    totper1ps
093500030822      *
093600030203     c                   setoff                                       7071
093700030203     c     totper1ps     comp      999                                70
093800030203     c  n70totper1ps     comp      -999                                 71
093900030203     c   70              z-add     999           totper1ps
094000030203     c   71              z-add     -999          totper1ps
094100030116     c                   else
094200030116     c                   z-add     0             totper1ps
094300020625     c                   endif
094400030115      *imposto i  campi x la stampa coop
094500030115     c                   eval      totcos1dsx =   totcos1dx
094600030115     c                   eval      totcom1dsx =   totcom1dx
094700030115     c                   eval      totcos1psx =   totcos1px
094800030115     c                   eval      totcom1psx =   totcom1px
094900030115     c                   eval      totcon1dsx =   totcon1dx
095000030115     c                   eval      totrit1dsx =   totrit1dx
095100030115     c                   eval      totcon1psx =   totcon1px
095200030115     c                   eval      totrit1psx =   totrit1px
095300030115     c                   eval      totcon1dcsx =  totcon1dcx
095400030115     c                   eval      totrit1dcsx =  totrit1dcx
095500030115     c                   eval      totcon1pcsx =  totcon1pcx
095600030115     c                   eval      totrit1pcsx =  totrit1pcx
095700030115
095800030522     c                   eval      totstm1psx =  totstm1px
095900030522     c                   eval      totstm1dsx =  totstm1dx
096000030522     c                   eval      totsco1sx = totstm1dx + totcos1dx
096100030522     c                   eval      totsco1psx = totstm1px + totcos1px
096200030522     c                   eval      totdif1sx = totcom1dx - totsco1sx
096300030522     c                   eval      totdif1psx = totcom1px - totsco1psx
096400030522
096500030203     c                   if        totcom1dx > 0
096600030822     c                   eval      totper18_2= ((totcom1dx - totsco1sx) * 100)
096700030115     c                                        / totcom1dx
096800030822     c                   Z-ADD     totper18_2    totper1sx
096900030822      *
097000030203     c                   setoff                                       7071
097100030203     c     totper1sx     comp      999                                70
097200030203     c  n70totper1sx     comp      -999                                 71
097300030203     c   70              z-add     999           totper1sx
097400030203     c   71              z-add     -999          totper1sx
097500030116     c                   else
097600030116     c                   z-add     0             totper1sx
097700030115     c                   endif
097800030115
097900030203     c                   if        totcom1px > 0
098000030822     c                   eval      totper18_2 = ((totcom1px - totsco1psx) * 100)
098100030115     c                                        / totcom1px
098200030822     c                   Z-ADD     totper18_2    totper1psx
098300030822      *
098400030203     c                   setoff                                       7071
098500030203     c     totper1psx    comp      999                                70
098600030203     c  n70totper1psx    comp      -999                                 71
098700030203     c   70              z-add     999           totper1psx
098800030203     c   71              z-add     -999          totper1psx
098900030116     c                   else
099000030116     c                   z-add     0             totper1psx
099100030115     c                   endif
099200030515      *imposto i  campi x la stampa affluenze defluenze
099300030515     c                   eval      totcos1dsa =   totcos1da
099400030515     c                   eval      totcom1dsa =   totcom1da
099500030515     c                   eval      totcos1psa =   totcos1pa
099600030515     c                   eval      totcom1psa =   totcom1pa
099700030515     c                   eval      totcon1dsa =   totcon1da
099800030515     c                   eval      totrit1dsa =   totrit1da
099900030515     c                   eval      totcon1psa =   totcon1pa
100000030515     c                   eval      totrit1psa =   totrit1pa
100100030515     c                   eval      totcon1dcsa =  totcon1dca
100200030515     c                   eval      totrit1dcsa =  totrit1dca
100300030515     c                   eval      totcon1pcsa =  totcon1pca
100400030515     c                   eval      totrit1pcsa =  totrit1pca
100500030515
100600030522     c                   eval      totstm1psa =  totstm1pa
100700030522     c                   eval      totstm1dsa =  totstm1da
100800030522     c                   eval      totsco1sa = totstm1da + totcos1da
100900030522     c                   eval      totsco1psa = totstm1pa + totcos1pa
101000030522     c                   eval      totdif1sa = totcom1da - totsco1sa
101100030522     c                   eval      totdif1psa = totcom1pa - totsco1psa
101200030522
101300030515     c                   if        totcom1da > 0
101400030822     c                   eval      totper18_2= ((totcom1da - totsco1sa) * 100)
101500030515     c                                        / totcom1da
101600030822     c                   Z-ADD     totper18_2    totper1sa
101700030822      *
101800030515     c                   setoff                                       7071
101900030515     c     totper1sa     comp      999                                70
102000030515     c  n70totper1sa     comp      -999                                 71
102100030515     c   70              z-add     999           totper1sa
102200030515     c   71              z-add     -999          totper1sa
102300030515     c                   else
102400030515     c                   z-add     0             totper1sa
102500030515     c                   endif
102600030515
102700030515     c                   if        totcom1pa > 0
102800030822     c                   eval      totper18_2 = ((totcom1pa - totsco1psa) * 100)
102900030515     c                                        / totcom1pa
103000030822     c                   Z-ADD     totper18_2    totper1psa
103100030822      *
103200030515     c                   setoff                                       7071
103300030515     c     totper1psa    comp      999                                70
103400030515     c  n70totper1psa    comp      -999                                 71
103500030515     c   70              z-add     999           totper1psa
103600030515     c   71              z-add     -999          totper1psa
103700030515     c                   else
103800030515     c                   z-add     0             totper1psa
103900030515     c                   endif
104000030528      * ricavo i totali per punto operativo
104100030528      *giornalieri
104200030528     c                   eval      stime1 = totstm1ds + totstm1dsx + totstm1dsa
104300030528     c                   eval      reali1 = totcos1ds + totcos1dsx + totcos1dsa
104400030528     c                   eval      costi1 = totsco1s + totsco1sx + totsco1sa
104500030528     c                   eval      compe1 = totcom1ds + totcom1dsx + totcom1dsa
104600030528     c                   eval      diffe1 = totdif1s + totdif1sx + totdif1sa
104700030528
104800030528      *periodo
104900030528     c                   eval      stime1p= totstm1ps + totstm1psx + totstm1psa
105000030528     c                   eval      reali1p= totcos1ps + totcos1psx + totcos1psa
105100030528     c                   eval      costi1p= totsco1ps + totsco1psx + totsco1psa
105200030528     c                   eval      compe1p= totcom1ps + totcom1psx + totcom1psa
105300030528     c                   eval      diffe1p= totdif1ps + totdif1psx + totdif1psa
105400030528      *calcola le percentuali
105500030528     c                   if        compe1  > 0
105600030822     c                   eval      totper18_2= ((compe1 -costi1) * 100) /compe1
105700030822     c                   Z-ADD     totper18_2    tpcompe1
105800030822      *
105900030528     c                   setoff                                       7071
106000030528     c     tpcompe1      comp      999                                70
106100030528     c  n70tpcompe1      comp      -999                                 71
106200030528     c   70              z-add     999           tpcompe1
106300030528     c   71              z-add     -999          tpcompe1
106400030528     c                   else
106500030528     c                   z-add     0             tpcompe1
106600030528     c                   endif
106700030528
106800030528     c                   if        compe1p > 0
106900030822     c                   eval      totper18_2 = ((compe1p-costi1p) * 100)
107000030822     c                                         /compe1p
107100030822     c                   Z-ADD     totper18_2    tpcompe1p
107200030822      *
107300030528     c                   setoff                                       7071
107400030528     c     tpcompe1p     comp      999                                70
107500030528     c  n70tpcompe1p     comp      -999                                 71
107600030528     c   70              z-add     999           tpcompe1p
107700030528     c   71              z-add     -999          tpcompe1p
107800030528     c                   else
107900030528     c                   z-add     0             tpcompe1p
108000030528     c                   endif
108100030922      *scrivo file riepiloghi se lavoro schedulato
108200030922     c                   if        schedulato <> *blank
108300030922     c                   exsr      riepiloghi
108400030922     c                   endif
108500030922
108600020625     c                   except    detta
108700030115      *azzero i campi livello filiale autotras.
108800020620     c                   clear                   totcos1d
108900020620     c                   clear                   totcom1d
109000020620     c                   clear                   totcos1p
109100020620     c                   clear                   totcom1p
109200020620     c                   clear                   totcon1d
109300020620     c                   clear                   totrit1d
109400020620     c                   clear                   totcon1p
109500020620     c                   clear                   totrit1p
109600020620     c                   clear                   totcon1dc
109700020620     c                   clear                   totrit1dc
109800020620     c                   clear                   totcon1pc
109900020620     c                   clear                   totrit1pc
110000030522
110100030522     c                   clear                   totstm1p
110200030522     c                   clear                   totstm1d
110300030115      *coop
110400030115     c                   clear                   totcos1dx
110500030115     c                   clear                   totcom1dx
110600030115     c                   clear                   totcos1px
110700030115     c                   clear                   totcom1px
110800030115     c                   clear                   totcon1dx
110900030115     c                   clear                   totrit1dx
111000030115     c                   clear                   totcon1px
111100030115     c                   clear                   totrit1px
111200030115     c                   clear                   totcon1dcx
111300030115     c                   clear                   totrit1dcx
111400030115     c                   clear                   totcon1pcx
111500030115     c                   clear                   totrit1pcx
111600030522
111700030522     c                   clear                   totstm1px
111800030522     c                   clear                   totstm1dx
111900030515      *aff/def
112000030515     c                   clear                   totcos1da
112100030515     c                   clear                   totcom1da
112200030515     c                   clear                   totcos1pa
112300030515     c                   clear                   totcom1pa
112400030515     c                   clear                   totcon1da
112500030515     c                   clear                   totrit1da
112600030515     c                   clear                   totcon1pa
112700030515     c                   clear                   totrit1pa
112800030515     c                   clear                   totcon1dca
112900030515     c                   clear                   totrit1dca
113000030515     c                   clear                   totcon1pca
113100030515     c                   clear                   totrit1pca
113200030522
113300030522     c                   clear                   totstm1pa
113400030522     c                   clear                   totstm1da
113500020620     c                   z-add     1             livello
113600020620     c                   end
113700020620      *rottura area
113800020620     c                   if        orgcar <> exarea
113900020625     c                   eval      totcos2ds =   totcos2d
114000020625     c                   eval      totcom2ds =   totcom2d
114100020625     c                   eval      totcos2ps =   totcos2p
114200020625     c                   eval      totcom2ps =   totcom2p
114300020625     c                   eval      totcon2ds =   totcon2d
114400020625     c                   eval      totrit2ds =   totrit2d
114500020625     c                   eval      totcon2ps =   totcon2p
114600020625     c                   eval      totrit2ps =   totrit2p
114700020625     c                   eval      totcon2dcs =   totcon2dc
114800020625     c                   eval      totrit2dcs =   totrit2dc
114900020625     c                   eval      totcon2pcs =   totcon2pc
115000020625     c                   eval      totrit2pcs =   totrit2pc
115100020625
115200030522     c                   eval      totstm2ps  =  totstm2p
115300030522     c                   eval      totstm2ds  =  totstm2d
115400030522     c                   eval      totsco2s  = totstm2d  + totcos2d
115500030522     c                   eval      totsco2ps  = totstm2p  + totcos2p
115600030522     c                   eval      totdif2s  = totcom2d  - totsco2s
115700030522     c                   eval      totdif2ps  = totcom2p  - totsco2ps
115800030522
115900030203     c                   if        totcom2d > 0
116000030822     c                   eval      totper18_2 = ((totcom2d - totsco2s) * 100)
116100020625     c                                        / totcom2d
116200030822     c                   Z-ADD     totper18_2    totper2s
116300030822      *
116400030203     c                   setoff                                       7071
116500030203     c     totper2s      comp      999                                70
116600030203     c  n70totper2s      comp      -999                                 71
116700030203     c   70              z-add     999           totper2s
116800030203     c   71              z-add     -999          totper2s
116900030116     c                   else
117000030116     c                   z-add     0             totper2s
117100020625     c                   endif
117200020625
117300030203     c                   if        totcom2p > 0
117400030822     c                   eval      totper18_2= ((totcom2p - totsco2ps) * 100)
117500020625     c                                        / totcom2p
117600030822     c                   Z-ADD     totper18_2    totper2ps
117700030822      *
117800030203     c                   setoff                                       7071
117900030203     c     totper2ps     comp      999                                70
118000030203     c  n70totper2ps     comp      -999                                 71
118100030203     c   70              z-add     999           totper2ps
118200030203     c   71              z-add     -999          totper2ps
118300030116     c                   else
118400030116     c                   z-add     0             totper2ps
118500020625     c                   endif
118600030115      *coop
118700030115     c                   eval      totcos2dsx =   totcos2dx
118800030115     c                   eval      totcom2dsx =   totcom2dx
118900030115     c                   eval      totcos2psx =   totcos2px
119000030115     c                   eval      totcom2psx =   totcom2px
119100030115     c                   eval      totcon2dsx =   totcon2dx
119200030115     c                   eval      totrit2dsx =   totrit2dx
119300030115     c                   eval      totcon2psx =   totcon2px
119400030115     c                   eval      totrit2psx =   totrit2px
119500030115     c                   eval      totcon2dcsx =   totcon2dcx
119600030115     c                   eval      totrit2dcsx =   totrit2dcx
119700030115     c                   eval      totcon2pcsx =   totcon2pcx
119800030115     c                   eval      totrit2pcsx =   totrit2pcx
119900030115
120000030522     c                   eval      totstm2psx =  totstm2px
120100030522     c                   eval      totstm2dsx =  totstm2dx
120200030522     c                   eval      totsco2sx = totstm2dx + totcos2dx
120300030522     c                   eval      totsco2psx = totstm2px + totcos2px
120400030522     c                   eval      totdif2sx = totcom2dx - totsco2sx
120500030522     c                   eval      totdif2psx = totcom2px - totsco2psx
120600030522
120700030203     c                   if        totcom2dx > 0
120800030822     c                   eval      totper18_2= ((totcom2dx - totsco2sx) * 100)
120900030115     c                                        / totcom2dx
121000030822     c                   Z-ADD     totper18_2    totper2sx
121100030822      *
121200030203     c                   setoff                                       7071
121300030203     c     totper2sx     comp      999                                70
121400030203     c  n70totper2sx     comp      -999                                 71
121500030203     c   70              z-add     999           totper2sx
121600030203     c   71              z-add     -999          totper2sx
121700030116     c                   else
121800030116     c                   z-add     0             totper2sx
121900030115     c                   endif
122000030115
122100030203     c                   if        totcom2px> 0
122200030822     c                   eval      totper18_2 = ((totcom2px - totsco2psx) * 100)
122300030115     c                                        / totcom2px
122400030822     c                   Z-ADD     totper18_2    totper2psx
122500030822      *
122600030203     c                   setoff                                       7071
122700030203     c     totper2psx    comp      999                                70
122800030203     c  n70totper2psx    comp      -999                                 71
122900030203     c   70              z-add     999           totper2psx
123000030203     c   71              z-add     -999          totper2psx
123100030116     c                   else
123200030116     c                   z-add     0             totper2psx
123300030115     c                   endif
123400030515      *aff/def
123500030515     c                   eval      totcos2dsa =   totcos2da
123600030515     c                   eval      totcom2dsa =   totcom2da
123700030515     c                   eval      totcos2psa =   totcos2pa
123800030515     c                   eval      totcom2psa =   totcom2pa
123900030515     c                   eval      totcon2dsa =   totcon2da
124000030515     c                   eval      totrit2dsa =   totrit2da
124100030515     c                   eval      totcon2psa =   totcon2pa
124200030515     c                   eval      totrit2psa =   totrit2pa
124300030515     c                   eval      totcon2dcsa =   totcon2dca
124400030515     c                   eval      totrit2dcsa =   totrit2dca
124500030515     c                   eval      totcon2pcsa =   totcon2pca
124600030515     c                   eval      totrit2pcsa =   totrit2pca
124700030515
124800030522     c                   eval      totstm2psa =  totstm2pa
124900030522     c                   eval      totstm2dsa =  totstm2da
125000030522     c                   eval      totsco2sa = totstm2da + totcos2da
125100030522     c                   eval      totsco2psa = totstm2pa + totcos2pa
125200030522     c                   eval      totdif2sa = totcom2da - totsco2sa
125300030522     c                   eval      totdif2psa = totcom2pa - totsco2psa
125400030522
125500030515     c                   if        totcom2da > 0
125600030822     c                   eval      totper18_2= ((totcom2da - totsco2sa) * 100)
125700030515     c                                        / totcom2da
125800030822     c                   Z-ADD     totper18_2    totper2sa
125900030822      *
126000030515     c                   setoff                                       7071
126100030515     c     totper2sa     comp      999                                70
126200030515     c  n70totper2sa     comp      -999                                 71
126300030515     c   70              z-add     999           totper2sa
126400030515     c   71              z-add     -999          totper2sa
126500030515     c                   else
126600030515     c                   z-add     0             totper2sa
126700030515     c                   endif
126800030515
126900030515     c                   if        totcom2pa> 0
127000030822     c                   eval      totper18_2 = ((totcom2pa - totsco2psa) * 100)
127100030515     c                                        / totcom2pa
127200030822     c                   Z-ADD     totper18_2    totper2psa
127300030822      *
127400030515     c                   setoff                                       7071
127500030515     c     totper2psa    comp      999                                70
127600030515     c  n70totper2psa    comp      -999                                 71
127700030515     c   70              z-add     999           totper2psa
127800030515     c   71              z-add     -999          totper2psa
127900030515     c                   else
128000030515     c                   z-add     0             totper2psa
128100030515     c                   endif
128200030528      * ricavo i totali per area
128300030528      *giornalieri
128400030528     c                   eval      stime2 = totstm2ds + totstm2dsx + totstm2dsa
128500030528     c                   eval      reali2 = totcos2ds + totcos2dsx + totcos2dsa
128600030528     c                   eval      costi2 = totsco2s + totsco2sx + totsco2sa
128700030528     c                   eval      compe2 = totcom2ds + totcom2dsx + totcom2dsa
128800030528     c                   eval      diffe2 = totdif2s + totdif2sx + totdif2sa
128900030528
129000030528      *periodo
129100030528     c                   eval      stime2p= totstm2ps + totstm2psx + totstm2psa
129200030528     c                   eval      reali2p= totcos2ps + totcos2psx + totcos2psa
129300030528     c                   eval      costi2p= totsco2ps + totsco2psx + totsco2psa
129400030528     c                   eval      compe2p= totcom2ps + totcom2psx + totcom2psa
129500030528     c                   eval      diffe2p= totdif2ps + totdif2psx + totdif2psa
129600030528      *calcola le percentuali
129700030528     c                   if        compe2  > 0
129800030822     c                   eval      totper18_2= ((compe2 -costi2) * 100) /compe2
129900030822     c                   Z-ADD     totper18_2    tpcompe2
130000030822      *
130100030528     c                   setoff                                       7071
130200030528     c     tpcompe2      comp      999                                70
130300030528     c  n70tpcompe2      comp      -999                                 71
130400030528     c   70              z-add     999           tpcompe2
130500030528     c   71              z-add     -999          tpcompe2
130600030528     c                   else
130700030528     c                   z-add     0             tpcompe2
130800030528     c                   endif
130900030528
131000030528     c                   if        compe2p > 0
131100030822     c                   eval      totper18_2=((compe2p-costi2p) * 100) /compe2p
131200030822     c                   Z-ADD     totper18_2    tpcompe2p
131300030822      *
131400030528     c                   setoff                                       7071
131500030528     c     tpcompe2p     comp      999                                70
131600030528     c  n70tpcompe2p     comp      -999                                 71
131700030528     c   70              z-add     999           tpcompe2p
131800030528     c   71              z-add     -999          tpcompe2p
131900030528     c                   else
132000030528     c                   z-add     0             tpcompe2p
132100030528     c                   endif
132200030528
132300020620     c                   except    area
132400020620     c                   z-add     66            lin2
132500020620      *azzero i campi livello area
132600020620     c                   clear                   totcos2d
132700020620     c                   clear                   totcom2d
132800020620     c                   clear                   totcos2p
132900020620     c                   clear                   totcom2p
133000020620     c                   clear                   totcon2d
133100020620     c                   clear                   totrit2d
133200020620     c                   clear                   totcon2p
133300020620     c                   clear                   totrit2p
133400020620     c                   clear                   totcon2dc
133500020620     c                   clear                   totrit2dc
133600020620     c                   clear                   totcon2pc
133700020620     c                   clear                   totrit2pc
133800030522
133900030522     c                   clear                   totstm2p
134000030522     c                   clear                   totstm2d
134100030115      *coop
134200030115     c                   clear                   totcos2dx
134300030115     c                   clear                   totcom2dx
134400030115     c                   clear                   totcos2px
134500030115     c                   clear                   totcom2px
134600030115     c                   clear                   totcon2dx
134700030115     c                   clear                   totrit2dx
134800030115     c                   clear                   totcon2px
134900030115     c                   clear                   totrit2px
135000030115     c                   clear                   totcon2dcx
135100030115     c                   clear                   totrit2dcx
135200030115     c                   clear                   totcon2pcx
135300030115     c                   clear                   totrit2pcx
135400030522
135500030522     c                   clear                   totstm2px
135600030522     c                   clear                   totstm2dx
135700030515      *aff/def
135800030515     c                   clear                   totcos2da
135900030515     c                   clear                   totcom2da
136000030515     c                   clear                   totcos2pa
136100030515     c                   clear                   totcom2pa
136200030515     c                   clear                   totcon2da
136300030515     c                   clear                   totrit2da
136400030515     c                   clear                   totcon2pa
136500030515     c                   clear                   totrit2pa
136600030515     c                   clear                   totcon2dca
136700030515     c                   clear                   totrit2dca
136800030515     c                   clear                   totcon2pca
136900030515     c                   clear                   totrit2pca
137000030522
137100030522     c                   clear                   totstm2pa
137200030522     c                   clear                   totstm2da
137300020620     c                   z-add     2             livello
137400020620     c                   end
137500020620
137600020620      *rottura distretto
137700020620     c                   if        orgfl3 <> exdistr
137800020625     c                   eval      totcos3ds =   totcos3d
137900020625     c                   eval      totcom3ds =   totcom3d
138000020625     c                   eval      totcos3ps =   totcos3p
138100020625     c                   eval      totcom3ps =   totcom3p
138200020625     c                   eval      totcon3ds =   totcon3d
138300020625     c                   eval      totrit3ds =   totrit3d
138400020625     c                   eval      totcon3ps =   totcon3p
138500020625     c                   eval      totrit3ps =   totrit3p
138600020625     c                   eval      totcon3dcs =  totcon3dc
138700020625     c                   eval      totrit3dcs =  totrit3dc
138800020625     c                   eval      totcon3pcs =  totcon3pc
138900020625     c                   eval      totrit3pcs =  totrit3pc
139000030522
139100030522     c                   eval      totstm3ps  =  totstm3p
139200030522     c                   eval      totstm3ds  =  totstm3d
139300030522     c                   eval      totsco3s  = totstm3d  + totcos3d
139400030522     c                   eval      totsco3ps  = totstm3p  + totcos3p
139500030522     c                   eval      totdif3s  = totcom3d  - totsco3s
139600030522     c                   eval      totdif3ps  = totcom3p  - totsco3ps
139700030522
139800030203     c                   if        totcom3d > 0
139900030822     c                   eval      totper18_2 = ((totcom3d - totsco3s) * 100)
140000020625     c                                        / totcom3d
140100030822     c                   Z-ADD     totper18_2    totper3s
140200030822      *
140300030203     c                   setoff                                       7071
140400030203     c     totper3s      comp      999                                70
140500030203     c  n70totper3s      comp      -999                                 71
140600030203     c   70              z-add     999           totper3s
140700030203     c   71              z-add     -999          totper3s
140800030116     c                   else
140900030116     c                   z-add     0             totper3s
141000020625     c                   endif
141100030203     c                   if        totcom3p > 0
141200030822     c                   eval      totper18_2= ((totcom3p - totsco3ps) * 100)
141300020625     c                                        / totcom3p
141400030822     c                   Z-ADD     totper18_2    totper3ps
141500030822      *
141600030203     c                   setoff                                       7071
141700030203     c     totper3ps     comp      999                                70
141800030203     c  n70totper3ps     comp      -999                                 71
141900030203     c   70              z-add     999           totper3ps
142000030203     c   71              z-add     -999          totper3ps
142100030116     c                   else
142200030116     c                   z-add     0             totper3ps
142300020625     c                   endif
142400030116      *coop
142500030116     c                   eval      totcos3dsx =   totcos3dx
142600030116     c                   eval      totcom3dsx =   totcom3dx
142700030116     c                   eval      totcos3psx =   totcos3px
142800030116     c                   eval      totcom3psx =   totcom3px
142900030116     c                   eval      totcon3dsx =   totcon3dx
143000030116     c                   eval      totrit3dsx =   totrit3dx
143100030116     c                   eval      totcon3psx =   totcon3px
143200030116     c                   eval      totrit3psx =   totrit3px
143300030116     c                   eval      totcon3dcsx =  totcon3dcx
143400030116     c                   eval      totrit3dcsx =  totrit3dcx
143500030116     c                   eval      totcon3pcsx =  totcon3pcx
143600030116     c                   eval      totrit3pcsx =  totrit3pcx
143700030522
143800030522     c                   eval      totstm3psx =  totstm3px
143900030522     c                   eval      totstm3dsx =  totstm3dx
144000030522     c                   eval      totsco3sx = totstm3dx + totcos3dx
144100030522     c                   eval      totsco3psx = totstm3px + totcos3px
144200030522     c                   eval      totdif3sx = totcom3dx - totsco3sx
144300030522     c                   eval      totdif3psx = totcom3px - totsco3psx
144400030203
144500030203     c                   if        totcom3dx > 0
144600030822     c                   eval      totper18_2= ((totcom3dx - totsco3sx) * 100)
144700030116     c                                        / totcom3dx
144800030822     c                   Z-ADD     totper18_2    totper3sx
144900030822      *
145000030203     c                   setoff                                       7071
145100030203     c     totper3sx     comp      999                                70
145200030203     c  n70totper3sx     comp      -999                                 71
145300030203     c   70              z-add     999           totper3sx
145400030203     c   71              z-add     -999          totper3sx
145500030116     c                   else
145600030116     c                   z-add     0             totper3sx
145700030116     c                   endif
145800030203     c                   if        totcom3px > 0
145900030822     c                   eval      totper18_2 = ((totcom3px - totsco3psx) * 100)
146000030116     c                                        / totcom3px
146100030822     c                   Z-ADD     totper18_2    totper3psx
146200030822      *
146300030203     c                   setoff                                       7071
146400030203     c     totper3psx    comp      999                                70
146500030203     c  n70totper3psx    comp      -999                                 71
146600030203     c   70              z-add     999           totper3psx
146700030203     c   71              z-add     -999          totper3psx
146800030116     c                   else
146900030116     c                   z-add     0             totper3psx
147000030116     c                   endif
147100030515      *aff/def
147200030515     c                   eval      totcos3dsa =   totcos3da
147300030515     c                   eval      totcom3dsa =   totcom3da
147400030515     c                   eval      totcos3psa =   totcos3pa
147500030515     c                   eval      totcom3psa =   totcom3pa
147600030515     c                   eval      totcon3dsa =   totcon3da
147700030515     c                   eval      totrit3dsa =   totrit3da
147800030515     c                   eval      totcon3psa =   totcon3pa
147900030515     c                   eval      totrit3psa =   totrit3pa
148000030515     c                   eval      totcon3dcsa =  totcon3dca
148100030515     c                   eval      totrit3dcsa =  totrit3dca
148200030515     c                   eval      totcon3pcsa =  totcon3pca
148300030515     c                   eval      totrit3pcsa =  totrit3pca
148400030515
148500030522     c                   eval      totstm3psa =  totstm3pa
148600030522     c                   eval      totstm3dsa =  totstm3da
148700030522     c                   eval      totsco3sa = totstm3da + totcos3da
148800030522     c                   eval      totsco3psa = totstm3pa + totcos3pa
148900030522     c                   eval      totdif3sa = totcom3da - totsco3sa
149000030522     c                   eval      totdif3psa = totcom3pa - totsco3psa
149100030522
149200030515     c                   if        totcom3da > 0
149300030822     c                   eval      totper18_2= ((totcom3da - totsco3sa) * 100)
149400030515     c                                        / totcom3da
149500030822     c                   Z-ADD     totper18_2    totper3sa
149600030822      *
149700030515     c                   setoff                                       7071
149800030515     c     totper3sa     comp      999                                70
149900030515     c  n70totper3sa     comp      -999                                 71
150000030515     c   70              z-add     999           totper3sa
150100030515     c   71              z-add     -999          totper3sa
150200030515     c                   else
150300030515     c                   z-add     0             totper3sa
150400030515     c                   endif
150500030515     c                   if        totcom3pa > 0
150600030822     c                   eval      totper18_2 = ((totcom3pa - totsco3psa) * 100)
150700030515     c                                        / totcom3pa
150800030822     c                   Z-ADD     totper18_2    totper3psa
150900030822      *
151000030515     c                   setoff                                       7071
151100030515     c     totper3psa    comp      999                                70
151200030515     c  n70totper3psa    comp      -999                                 71
151300030515     c   70              z-add     999           totper3psa
151400030515     c   71              z-add     -999          totper3psa
151500030515     c                   else
151600030515     c                   z-add     0             totper3psa
151700030515     c                   endif
151800030528      * ricavo i totali per distretto
151900030528      *giornalieri
152000030528     c                   eval      stime3 = totstm3ds + totstm3dsx + totstm3dsa
152100030528     c                   eval      reali3 = totcos3ds + totcos3dsx + totcos3dsa
152200030528     c                   eval      costi3 = totsco3s + totsco3sx + totsco3sa
152300030528     c                   eval      compe3 = totcom3ds + totcom3dsx + totcom3dsa
152400030528     c                   eval      diffe3 = totdif3s + totdif3sx + totdif3sa
152500030528
152600030528      *periodo
152700030528     c                   eval      stime3p= totstm3ps + totstm3psx + totstm3psa
152800030528     c                   eval      reali3p= totcos3ps + totcos3psx + totcos3psa
152900030528     c                   eval      costi3p= totsco3ps + totsco3psx + totsco3psa
153000030528     c                   eval      compe3p= totcom3ps + totcom3psx + totcom3psa
153100030528     c                   eval      diffe3p= totdif3ps + totdif3psx + totdif3psa
153200030528      *calcola le percentuali
153300030528     c                   if        compe3  > 0
153400030822     c                   eval      totper18_2= ((compe3 -costi3) * 100) /compe3
153500030822     c                   Z-ADD     totper18_2    tpcompe3
153600030822      *
153700030528     c                   setoff                                       7071
153800030528     c     tpcompe3      comp      999                                70
153900030528     c  n70tpcompe3      comp      -999                                 71
154000030528     c   70              z-add     999           tpcompe3
154100030528     c   71              z-add     -999          tpcompe3
154200030528     c                   else
154300030528     c                   z-add     0             tpcompe3
154400030528     c                   endif
154500030528
154600030528     c                   if        compe3p > 0
154700030822     c                   eval      totper18_2=((compe3p-costi3p) * 100) /compe3p
154800030822     c                   Z-ADD     totper18_2    tpcompe3p
154900030822      *
155000030528     c                   setoff                                       7071
155100030528     c     tpcompe3p     comp      999                                70
155200030528     c  n70tpcompe3p     comp      -999                                 71
155300030528     c   70              z-add     999           tpcompe3p
155400030528     c   71              z-add     -999          tpcompe3p
155500030528     c                   else
155600030528     c                   z-add     0             tpcompe3p
155700030528     c                   endif
155800030528
155900020620     c                   except    distret
156000020620     c                   z-add     66            lin3
156100030114      *azzero i campi livello distretto
156200030116     c                   clear                   totcos3d
156300030116     c                   clear                   totcom3d
156400030116     c                   clear                   totcos3p
156500030116     c                   clear                   totcom3p
156600030116     c                   clear                   totcon3d
156700030116     c                   clear                   totrit3d
156800030116     c                   clear                   totcon3p
156900030116     c                   clear                   totrit3p
157000030116     c                   clear                   totcon3dc
157100030116     c                   clear                   totrit3dc
157200020620     c                   clear                   totcon3pc
157300030116     c                   clear                   totrit3pc
157400030522
157500030522     c                   clear                   totstm3p
157600030522     c                   clear                   totstm3d
157700030116      *coop
157800030116     c                   clear                   totcos3dx
157900030116     c                   clear                   totcom3dx
158000030116     c                   clear                   totcos3px
158100030116     c                   clear                   totcom3px
158200030116     c                   clear                   totcon3dx
158300030116     c                   clear                   totrit3dx
158400030116     c                   clear                   totcon3px
158500030116     c                   clear                   totrit3px
158600030116     c                   clear                   totcon3dcx
158700030116     c                   clear                   totrit3dcx
158800030116     c                   clear                   totcon3pcx
158900030116     c                   clear                   totrit3pcx
159000030522
159100030522     c                   clear                   totstm3px
159200030522     c                   clear                   totstm3dx
159300030515      *aff/def
159400030515     c                   clear                   totcos3da
159500030515     c                   clear                   totcom3da
159600030515     c                   clear                   totcos3pa
159700030515     c                   clear                   totcom3pa
159800030515     c                   clear                   totcon3da
159900030515     c                   clear                   totrit3da
160000030515     c                   clear                   totcon3pa
160100030515     c                   clear                   totrit3pa
160200030515     c                   clear                   totcon3dca
160300030515     c                   clear                   totrit3dca
160400030515     c                   clear                   totcon3pca
160500030515     c                   clear                   totrit3pca
160600030522
160700030522     c                   clear                   totstm3pa
160800030522     c                   clear                   totstm3da
160900030116     c                   z-add     3             livello
161000020620     c                   end
161100020620
161200020620     c                   exsr      invia
161300100210     c                   end
161400020620     c                   endsr
161500020521      *------------------------------------------------------
161600020521     c     elabora       begsr
161700020521      *------------------------------------------------------
161800020611     c                   clear                   rekok
161900030606     c                   clear                   exfssddc
162000030115      *elabora prima i totali coop per la filiale
162100030115     c                   exsr      elaboracoop
162200030115
162300030114      *loop record saldi autotrasportatori
162400020612     c     kfss          setll     fifss01l
162500020517     c                   do        *hival
162600020612     c                   read      fifss01l                               98
162700020517     c   98              leave
162800030114      *rottura data o filiale
162900020611     c                   if        fssddc > alladat or fssfgs <> orgfil
163000020611     c                   leave
163100020611     c                   endif
163200020612
163300030115      *imposta campi per le rotture
163400020612     c                   if        rekok = *blank
163500020612     c                   select
163600020612     c                   when      livello = 3
163700020612     c                   move      orgfl3        exdistr
163800020612     c                   move      orgcar        exarea
163900020612     c                   move      orgfil        exfil
164000020612     c                   when      livello = 2
164100020612     c                   move      orgcar        exarea
164200020612     c                   move      orgfil        exfil
164300020612     c                   when      livello = 1
164400020612     c                   move      orgfil        exfil
164500020612     c                   endsl
164600020619
164700020619     c                   movel     orgdes        desfil
164800020619
164900020612     c                   exsr      imbuca
165000020612     c                   endif
165100020611
165200020611     c                   move      'X'           rekok
165300030516      *totalizza stime autotrasportatori per giornata
165400030527      *in questo caso controllo la data perchè per la stessa ho più rek
165500030527      *con codici aut. diversi le stime devo prenderle una sola volta x data
165600030527     c                   if        fssddc <> exfssddc
165700030527     c                   move      fssddc        exfssddc
165800030515     c                   move      *zeros        kfrepdr
165900030515     c                   movel     orgfil        kfrepdr
166000030527     c                   move      fssddc        kfreddc
166100030515     c                   move      'A'           kfretsr
166200030526     c                   move      'G'           kfretip
166300030515     c                   clear                   importo
166400030515     c                   exsr      stime
166500030527     c                   else
166600030527     c                   clear                   importo
166700030527     c                   end
166800080924
166900080924     c* somma nei costi stimati eventuali aumenti costo carburante
167000080924     c                   add       fssxx1        importo
167100020612
167200020614      *totalizza i costi del record
167300020620     c                   eval      cosparz  = (fss001 + fss005
167400020614     c                             + fss016 + fss019 + fss034 + fss035
167500020620     c                             + fss997 + fss998)
167600020614
167700020614      *totalizza le competenze del record
167800020620     c                   eval      comparz  = (fss001b + fss005b
167900020614     c                             + fss016b + fss019b + fss034b + fss035b
168000030430     c                             + fss997b + fss998b + fssxx1b + fssxx2b)
168100020614
168200020619      *totalizza i costi solo consegne
168300020620     c                   eval      conparz  = (fss005 + fss016
168400020620     c                             + fss019 + fss034 + fss998)
168500020619
168600020619      *totalizza i costi solo ritiri
168700020620     c                   eval      ritparz = (fss001 +fss035 + fss997)
168800020619
168900020619      *totalizza le competenze solo consegne
169000020620     c                   eval      conparzc = (fss005b +  + fss016b
169100030430     c                             + fss019b + fss034b + fss998b
169200030430     c                             + fssxx1b + fssxx2b)
169300020619
169400020619      *totalizza le competenze solo ritiri
169500020620     c                   eval      ritparzc = (fss001b + fss035b)
169600020619
169700020619      *controllo  per sapere dove totalizzare se nel periodo o alla data
169800030225     c                   if        fssddc >= alladatx
169900030515      *stime giorno in esame
170000030515     c                   add       importo       totstm1d
170100030515     c                   add       importo       totstm2d
170200030515     c                   add       importo       totstm3d
170300030515     c                   add       importo       totstm4d
170400020614      *costi giorno in esame
170500020614     c                   add       cosparz       totcos1d
170600020614     c                   add       cosparz       totcos2d
170700020614     c                   add       cosparz       totcos3d
170800020614     c                   add       cosparz       totcos4d
170900020614      *competenze giorno in esame
171000020614     c                   add       comparz       totcom1d
171100020614     c                   add       comparz       totcom2d
171200020614     c                   add       comparz       totcom3d
171300020614     c                   add       comparz       totcom4d
171400020619      *costi giorno in esame solo consegne
171500020619     c                   add       conparz       totcon1d
171600020619     c                   add       conparz       totcon2d
171700020619     c                   add       conparz       totcon3d
171800020619     c                   add       conparz       totcon4d
171900020619      *costi giorno in esame solo ritiri
172000020619     c                   add       ritparz       totrit1d
172100020619     c                   add       ritparz       totrit2d
172200020619     c                   add       ritparz       totrit3d
172300020619     c                   add       ritparz       totrit4d
172400020619      *competenze giorno in esame solo consegne
172500020619     c                   add       conparzc      totcon1dc
172600020619     c                   add       conparzc      totcon2dc
172700020619     c                   add       conparzc      totcon3dc
172800020619     c                   add       conparzc      totcon4dc
172900020619      *competenze giorno in esame solo ritiri
173000020619     c                   add       ritparzc      totrit1dc
173100020619     c                   add       ritparzc      totrit2dc
173200020619     c                   add       ritparzc      totrit3dc
173300020619     c                   add       ritparzc      totrit4dc
173400030225     c                   endif
173500020619      * totalizzazione nel periodo
173600030225     c*                  else
173700030515      *stime periodo  in esame
173800030515     c                   add       importo       totstm1p
173900030515     c                   add       importo       totstm2p
174000030515     c                   add       importo       totstm3p
174100030515     c                   add       importo       totstm4p
174200020614      *costi progressivo periodo
174300020614     c                   add       cosparz       totcos1p
174400020614     c                   add       cosparz       totcos2p
174500020614     c                   add       cosparz       totcos3p
174600020614     c                   add       cosparz       totcos4p
174700020614      *competenze progressivo periodo
174800020619     c                   add       comparz       totcom1p
174900020619     c                   add       comparz       totcom2p
175000020619     c                   add       comparz       totcom3p
175100020619     c                   add       comparz       totcom4p
175200020619      *costi periodo in esame solo consegne
175300020619     c                   add       conparz       totcon1p
175400020619     c                   add       conparz       totcon2p
175500020619     c                   add       conparz       totcon3p
175600020619     c                   add       conparz       totcon4p
175700020619      *costi periodo in esame solo ritiri
175800020619     c                   add       ritparz       totrit1p
175900020619     c                   add       ritparz       totrit2p
176000020619     c                   add       ritparz       totrit3p
176100020619     c                   add       ritparz       totrit4p
176200020619      *competenze periodo in esame solo consegne
176300020620     c                   add       conparzc      totcon1pc
176400020620     c                   add       conparzc      totcon2pc
176500020620     c                   add       conparzc      totcon3pc
176600020620     c                   add       conparzc      totcon4pc
176700020619      *competenze periodo in esame solo ritiri
176800020620     c                   add       ritparzc      totrit1pc
176900020620     c                   add       ritparzc      totrit2pc
177000020620     c                   add       ritparzc      totrit3pc
177100020620     c                   add       ritparzc      totrit4pc
177200030225     c*                  end
177300020611     c
177400020517     c                   enddo
177401100210     c                   if        rekok = *blank and orgfl3 < '0'
177402100210     c                   select
177403100210     c                   when      livello = 3
177404100210     c                   move      orgfl3        exdistr
177405100210     c                   move      orgcar        exarea
177406100210     c                   move      orgfil        exfil
177410100210     c                   when      livello = 2
177411100210     c                   move      orgcar        exarea
177412100210     c                   move      orgfil        exfil
177415100210     c                   when      livello = 1
177416100210     c                   move      orgfil        exfil
177418100210     c                   endsl
177419100210
177420100210     c                   movel     'F'           rekok
177421100210     c                   movel     orgdes        desfil
177422100210     c                   exsr      imbuca
177423100210
177424100210     c                   endif
177500020521     c                   endsr
177600030115      *------------------------------------------------------
177700030115     c     elaboracoop   begsr
177800030115      *------------------------------------------------------
177900030526     c                   exsr      elaboraAFDE
178000030115
178100030115      *loop record saldi coop
178200030115     c     kfss          setll     fifsc01l
178300030115     c                   do        *hival
178400030115     c                   read      fifsc01l                               98
178500030115     c   98              leave
178600030115      *rottura data o filiale
178700030115     c                   if        fscddc > alladat or fscfgs <> orgfil
178800030115     c                   leave
178900030116     c                   endif
179000030516      *totalizza stime coop  per giornata
179100061004     c                   move      *zeros        kfrepdr
179200061004     c                   movel     orgfil        kfrepdr
179300061004     c*mm                move      fscpdr        kfrepdr
179400030527     c                   move      fscddc        kfreddc
179500030515     c                   move      'F'           kfretsr
179600030526     c                   move      'C'           kfretip
179700030515     c                   clear                   importo
179800030515     c                   exsr      stime
179900030115
180000030115      *totalizza i costi del record
180100030115     c                   eval      cosparzx = (fsc002 + fsc005
180200030115     c                             + fsc006 + fsc009 + fsc010 + fsc011
180300030115     c                             + fsc012 + fsc023 + fsc039 + fsc040
180400030115     c                             + fsc041 + fsc060 + fsc061 + fscxx1
180500030115     c                             + fscxx2)
180600030115
180700030115      *totalizza le competenze del record
180800030115     c                   eval      comparzx = (fsc002b + fsc005b
180900030115     c                             + fsc006b + fsc009b + fsc010b + fsc011b
181000030115     c                             + fsc012b + fsc023b + fsc039b + fsc040b
181100030115     c                             + fsc041b + fsc060b + fsc061b + fscxx1b
181200030115     c                             + fscxx2b)
181300030115
181400030115      *totalizza i costi solo consegne
181500030115     c                   eval      conparzx = (fsc005 + fsc006 + fsc041
181600030115     c                             + fsc010 + fsc011 + fsc012
181700030115     c                             + fsc023 + fsc060 + fsc061)
181800030115
181900030115      *totalizza i costi solo ritiri
182000030115     c                   eval      ritparzx = (fsc002 +fsc009
182100030115     c                             + fsc039 + fsc040)
182200030115
182300030115      *totalizza le competenze solo consegne
182400030115     c                   eval      conparzcx = (fsc005b+ fsc006b+ fsc041b
182500030115     c                             + fsc010b + fsc011b + fsc012b
182600030115     c                             + fsc023b + fsc060b + fsc061b)
182700030115
182800030115      *totalizza le competenze solo ritiri
182900030115     c                   eval      ritparzcx = (fsc002b + fsc009b
183000030115     c                             + fsc039b + fsc040b)
183100030115
183200030115      *controllo  per sapere dove totalizzare se nel periodo o alla data
183300030225     c                   if        fscddc >= alladatx
183400030515      *stime giorno in esame
183500030515     c                   add       importo       totstm1dx
183600030515     c                   add       importo       totstm2dx
183700030515     c                   add       importo       totstm3dx
183800030515     c                   add       importo       totstm4dx
183900030115      *costi giorno in esame
184000030115     c                   add       cosparzx      totcos1dx
184100030115     c                   add       cosparzx      totcos2dx
184200030115     c                   add       cosparzx      totcos3dx
184300030115     c                   add       cosparzx      totcos4dx
184400030115      *competenze giorno in esame
184500030115     c                   add       comparzx      totcom1dx
184600030115     c                   add       comparzx      totcom2dx
184700030115     c                   add       comparzx      totcom3dx
184800030115     c                   add       comparzx      totcom4dx
184900030115      *costi giorno in esame solo consegne
185000030115     c                   add       conparzx      totcon1dx
185100030115     c                   add       conparzx      totcon2dx
185200030115     c                   add       conparzx      totcon3dx
185300030115     c                   add       conparzx      totcon4dx
185400030115      *costi giorno in esame solo ritiri
185500030115     c                   add       ritparzx      totrit1dx
185600030115     c                   add       ritparzx      totrit2dx
185700030115     c                   add       ritparzx      totrit3dx
185800030115     c                   add       ritparzx      totrit4dx
185900030115      *competenze giorno in esame solo consegne
186000030115     c                   add       conparzcx     totcon1dcx
186100030115     c                   add       conparzcx     totcon2dcx
186200030115     c                   add       conparzcx     totcon3dcx
186300030115     c                   add       conparzcx     totcon4dcx
186400030115      *competenze giorno in esame solo ritiri
186500030115     c                   add       ritparzcx     totrit1dcx
186600030115     c                   add       ritparzcx     totrit2dcx
186700030115     c                   add       ritparzcx     totrit3dcx
186800030115     c                   add       ritparzcx     totrit4dcx
186900030225     c                   endif
187000030115      * totalizzazione nel periodo
187100030225     c*                  else
187200030515      *stime periodo in esame
187300030515     c                   add       importo       totstm1px
187400030515     c                   add       importo       totstm2px
187500030515     c                   add       importo       totstm3px
187600030515     c                   add       importo       totstm4px
187700030115      *costi progressivo periodo
187800030115     c                   add       cosparzx      totcos1px
187900030115     c                   add       cosparzx      totcos2px
188000030115     c                   add       cosparzx      totcos3px
188100030115     c                   add       cosparzx      totcos4px
188200030115      *competenze progressivo periodo
188300030115     c                   add       comparzx      totcom1px
188400030115     c                   add       comparzx      totcom2px
188500030115     c                   add       comparzx      totcom3px
188600030115     c                   add       comparzx      totcom4px
188700030115      *costi periodo in esame solo consegne
188800030115     c                   add       conparzx      totcon1px
188900030115     c                   add       conparzx      totcon2px
189000030115     c                   add       conparzx      totcon3px
189100030115     c                   add       conparzx      totcon4px
189200030115      *costi periodo in esame solo ritiri
189300030115     c                   add       ritparzx      totrit1px
189400030115     c                   add       ritparzx      totrit2px
189500030115     c                   add       ritparzx      totrit3px
189600030115     c                   add       ritparzx      totrit4px
189700030115      *competenze periodo in esame solo consegne
189800030115     c                   add       conparzcx     totcon1pcx
189900030115     c                   add       conparzcx     totcon2pcx
190000030115     c                   add       conparzcx     totcon3pcx
190100030115     c                   add       conparzcx     totcon4pcx
190200030115      *competenze periodo in esame solo ritiri
190300030115     c                   add       ritparzcx     totrit1pcx
190400030115     c                   add       ritparzcx     totrit2pcx
190500030115     c                   add       ritparzcx     totrit3pcx
190600030115     c                   add       ritparzcx     totrit4pcx
190700030225     c*                  end
190800030115     c
190900030115     c                   enddo
191000030115     c                   endsr
191100030515      *------------------------------------------------------
191200030515     c     elaboraAFde   begsr
191300030515      *------------------------------------------------------
191400030515
191500030515      *loop record saldi coop
191600030522     c     kfss          setll     fifsa01l
191700030515     c                   do        *hival
191800030515     c                   read      fifsa01l                               98
191900030515     c   98              leave
192000030515      *rottura data o filiale
192100030515     c                   if        fsaddc > alladat or fsafgs <> orgfil
192200030515     c                   leave
192300030515     c                   endif
192400030515
192500030515      *totalizza stime aff/def  per giornata
192600030515     c                   move      *zeros        kfrepdr
192700030515     c                   movel     orgfil        kfrepdr
192800030515     c                   move      fsaddc        kfreddc
192900030515     c                   move      'D'           kfretsr
193000030526     c                   move      'G'           kfretip
193100030515     c                   clear                   importo
193200030515     c                   exsr      stime
193300080918     c* somma nei costi stimati eventuali aumenti costo carburante
193400080918     c                   add       fsaxx5        importo
193500030515      *totalizza i costi del record
193600030515     c                   eval      cosparza = (fsa014 + fsa015
193700030515     c                             + fsa046 + fsa047 + fsa049 + fsa050
193800080918     c                             + fsaxx1 + fsaxx2 + fsaxx3 + fsaxx4)
193900030515
194000030515      *totalizza le competenze del record
194100030515     c                   eval      comparza = (fsa014b + fsa015b
194200030515     c                             + fsa046b + fsa047b + fsa049b + fsa050b
194300080918     c                             + fsaxx1b + fsaxx2b + fsaxx3b + fsaxx4b)
194400030515
194500030515      *totalizza i costi solo consegne
194600030515     c                   eval      conparza = (fsa015 + fsa046 + fsa049)
194700030515
194800030515      *totalizza i costi solo ritiri
194900030515     c                   eval      ritparza = (fsa014 +fsa047 + fsa050)
195000030515
195100030515      *totalizza le competenze solo consegne
195200030515     c                   eval      conparzca = (fsa015b+ fsa046b+ fsa049b)
195300030515
195400030515      *totalizza le competenze solo ritiri
195500030515     c                   eval      ritparzca = (fsa014b + fsa046b + fsa050b)
195600030515
195700030515      *controllo  per sapere dove totalizzare se nel periodo o alla data
195800030526     c                   if        fsaddc >= alladatx
195900030515      *stime giorno in esame
196000030515     c                   add       importo       totstm1da
196100030515     c                   add       importo       totstm2da
196200030515     c                   add       importo       totstm3da
196300030515     c                   add       importo       totstm4da
196400030515      *costi giorno in esame
196500030515     c                   add       cosparza      totcos1da
196600030515     c                   add       cosparza      totcos2da
196700030515     c                   add       cosparza      totcos3da
196800030515     c                   add       cosparza      totcos4da
196900030515      *competenze giorno in esame
197000030515     c                   add       comparza      totcom1da
197100030515     c                   add       comparza      totcom2da
197200030515     c                   add       comparza      totcom3da
197300030515     c                   add       comparza      totcom4da
197400030515      *costi giorno in esame solo consegne
197500030515     c                   add       conparza      totcon1da
197600030515     c                   add       conparza      totcon2da
197700030515     c                   add       conparza      totcon3da
197800030515     c                   add       conparza      totcon4da
197900030515      *costi giorno in esame solo ritiri
198000030515     c                   add       ritparza      totrit1da
198100030515     c                   add       ritparza      totrit2da
198200030515     c                   add       ritparza      totrit3da
198300030515     c                   add       ritparza      totrit4da
198400030515      *competenze giorno in esame solo consegne
198500030515     c                   add       conparzca     totcon1dca
198600030515     c                   add       conparzca     totcon2dca
198700030515     c                   add       conparzca     totcon3dca
198800030515     c                   add       conparzca     totcon4dca
198900030515      *competenze giorno in esame solo ritiri
199000030515     c                   add       ritparzca     totrit1dca
199100030515     c                   add       ritparzca     totrit2dca
199200030515     c                   add       ritparzca     totrit3dca
199300030515     c                   add       ritparzca     totrit4dca
199400030515     c                   endif
199500030515      * totalizzazione nel periodo
199600030515     c*                  else
199700030515      *stime progressivo in esame
199800030515     c                   add       importo       totstm1pa
199900030515     c                   add       importo       totstm2pa
200000030515     c                   add       importo       totstm3pa
200100030515     c                   add       importo       totstm4pa
200200030515      *costi progressivo periodo
200300030515     c                   add       cosparza      totcos1pa
200400030515     c                   add       cosparza      totcos2pa
200500030515     c                   add       cosparza      totcos3pa
200600030515     c                   add       cosparza      totcos4pa
200700030515      *competenze progressivo periodo
200800030515     c                   add       comparza      totcom1pa
200900030515     c                   add       comparza      totcom2pa
201000030515     c                   add       comparza      totcom3pa
201100030515     c                   add       comparza      totcom4pa
201200030515      *costi periodo in esame solo consegne
201300030515     c                   add       conparza      totcon1pa
201400030515     c                   add       conparza      totcon2pa
201500030515     c                   add       conparza      totcon3pa
201600030515     c                   add       conparza      totcon4pa
201700030515      *costi periodo in esame solo ritiri
201800030515     c                   add       ritparza      totrit1pa
201900030515     c                   add       ritparza      totrit2pa
202000030515     c                   add       ritparza      totrit3pa
202100030515     c                   add       ritparza      totrit4pa
202200030515      *competenze periodo in esame solo consegne
202300030515     c                   add       conparzca     totcon1pca
202400030515     c                   add       conparzca     totcon2pca
202500030515     c                   add       conparzca     totcon3pca
202600030515     c                   add       conparzca     totcon4pca
202700030515      *competenze periodo in esame solo ritiri
202800030515     c                   add       ritparzca     totrit1pca
202900030515     c                   add       ritparzca     totrit2pca
203000030515     c                   add       ritparzca     totrit3pca
203100030515     c                   add       ritparzca     totrit4pca
203200030515     c*                  end
203300030515     c
203400030515     c                   enddo
203500030515     c                   endsr
203600030515      *------------------------------------------------------
203700030515     c     stime         begsr
203800030515      *------------------------------------------------------
203900030515      * esegue totalizzazione stime per tipologia
204000030515     c     kfre          setll     fifre02l
204100030515     c                   do        *hival
204200030515     c     kfre          reade     fifre02l                               97
204300030515     c   97              leave
204400030515     c                   add       fretim        importo
204500030515     c                   enddo
204600030515     c                   endsr
204700020521      *------------------------------------------------------
204800020611     c     invia         begsr
204900020521      *------------------------------------------------------
205000020612      *deve eseguire l'invio nelle code apposite
205100020612
205200020612     c                   select
205300020612     c                   when      livello = 3
205400020612     c                   close     ecoprt3
205500020612     c                   close     ecoprt2
205600020612     c                   close     ecoprt1
205700020612     c                   when      livello = 2
205800020612     c                   close     ecoprt2
205900020612     c                   close     ecoprt1
206000020612     c                   when      livello = 1
206100020612     c                   close     ecoprt1
206200020612     c                   endsl
206300020521
206400020521     c                   endsr
206500020611      *------------------------------------------------------
206600020611     c     imbuca        begsr
206700020611      *------------------------------------------------------
206800020612      *deve overridare i printer con le nuove indicazioni di istradamento
206900020612     c                   select
207000020612      *rottura distretto
207100020612     c                   when      livello = 3
207200020612      * override per distretto
207300020612     c                   z-add     3             lvl
207400020619      *reperimento coda output
207500020619     c                   move      '17'          tblcod
207600030227     c                   movel(p)  exdistr       tblkey
207700020619     c     ktab          chain     tabel00f                           90
207800020619     c                   movel     tbluni        ds17
207900030227     c                   movel     §17des        exdesd           15
208000020619      *imposta per override
208100020619     c                   movel     §17otq        PrmOutq
208200020612     c                   exsr      openovr
208300020612     c                   open      ecoprt3
208400020612      * override per area
208500020612     c                   z-add     2             lvl
208600020619      *reperimento coda output
208700020619     c                   move      '05'          tblcod
208800020619     c                   movel     exarea        tblkey
208900020619     c     ktab          chain     tabel00f                           90
209000020619     c                   movel     tbluni        ds05
209100030227     c                   movel     §05des        exdesa           18
209200020619      *imposta per override
209300020619     c                   movel     §05otq        PrmOutq
209400020612     c                   exsr      openovr
209500020612     c                   open      ecoprt2
209600020612      * override per P.O.
209700020612     c                   z-add     1             lvl
209800030513     c                   if        orgfag = 'A'
209900030513     c                   move      outqsede      prmoutq
210000030513     c                   else
210100020619     c                   movel     orgde4        PrmOutq
210200030513     c                   endif
210300020612     c                   exsr      openovr
210400020612     c                   open      ecoprt1
210500020612      *rottura area
210600020612     c                   when      livello = 2
210700020612      * override per area
210800020612     c                   z-add     2             lvl
210900020619      *reperimento coda output
211000020619     c                   move      '05'          tblcod
211100020619     c                   movel     exarea        tblkey
211200020619     c     ktab          chain     tabel00f                           90
211300020619     c                   movel     tbluni        ds05
211400030227     c                   movel     §05des        exdesa
211500020619      *imposta per override
211600020619     c                   movel     §05otq        PrmOutq
211700020612     c                   exsr      openovr
211800020612     c                   open      ecoprt2
211900020612      * override per P.O.
212000020612     c                   z-add     1             lvl
212100030513     c                   if        orgfag = 'A'
212200030513     c                   move      outqsede      prmoutq
212300030513     c                   else
212400020619     c                   movel     orgde4        PrmOutq
212500030513     c                   endif
212600020612     c                   exsr      openovr
212700020612     c                   open      ecoprt1
212800020612      *rottura area
212900020612     c                   when      livello = 1
213000020612      * override per P.O.
213100020612     c                   z-add     1             lvl
213200030513     c                   if        orgfag = 'A'
213300030513     c                   move      outqsede      prmoutq
213400030513     c                   else
213500020619     c                   movel     orgde4        PrmOutq
213600030513     c                   endif
213700020612     c                   exsr      openovr
213800020612     c                   open      ecoprt1
213900020612
214000020612     c                   endsl
214100020611
214200020611     c                   endsr
214300030922     C*----------------------------------------------------*
214400030922     C     riepiloghi    begsr
214500030922     C*----------------------------------------------------*
214600030922     c                   move      exfil         fsrfgs
214700031006     c                   z-add     alladat       fsrddc
214800030922     c                   move      *blank        fsrflr
214900030922      *record autotrasportatori
215000030922     c                   move      'A'           fsrtpr
215100030922     c                   z-add     totstm1d      fsrstg
215200030922     c                   z-add     totcos1d      fsrcog
215300030922     c                   z-add     totcom1d      fsrcmg
215400030922     c                   z-add     totstm1p      fsrstp
215500030922     c                   z-add     totcos1p      fsrcop
215600030922     c                   z-add     totcom1p      fsrcmp
215700030922     c                   write     fifsr000
215800030922      *record coop
215900030922     c                   move      'C'           fsrtpr
216000030922     c                   z-add     totstm1dx     fsrstg
216100030922     c                   z-add     totcos1dx     fsrcog
216200030922     c                   z-add     totcom1dx     fsrcmg
216300030922     c                   z-add     totstm1px     fsrstp
216400030922     c                   z-add     totcos1px     fsrcop
216500030922     c                   z-add     totcom1px     fsrcmp
216600030922     c                   write     fifsr000
216700030922      *record aff/Def
216800030922     c                   move      'D'           fsrtpr
216900030922     c                   z-add     totstm1da     fsrstg
217000030922     c                   z-add     totcos1da     fsrcog
217100030922     c                   z-add     totcom1da     fsrcmg
217200030922     c                   z-add     totstm1pa     fsrstp
217300030922     c                   z-add     totcos1pa     fsrcop
217400030922     c                   z-add     totcom1pa     fsrcmp
217500030922     c                   write     fifsr000
217600030922     c                   endsr
217700020612     C*----------------------------------------------------*
217800020612     C     openovr       begsr
217900020612     C*----------------------------------------------------*
218000020620     C                   z-add     62            lung             15 5
218100020620     C                   clear                   comman
218200020612      *ovr su outq di filiale
218300030403     c                   if        dOvr = 'N'
218400030403     C                   EVAL      %SUBST(cmd:52:10) = OutqSede
218500030403     c                   else
218600020620     C                   EVAL      %SUBST(cmd:52:10) = PrmOutq
218700030403     c                   end
218800020612     C                   movea     cmd(lvl)      comman           80
218900020612     C                   call      'QCMDEXC'                            20
219000020612     C                   parm                    comman
219100020612     C                   parm                    lung
219200020612     C*
219300020612     c                   endsr
219400020619     C*----------------------------------------------------*
219500020619     C     testate       begsr
219600020619     C*----------------------------------------------------*
219700020619     c                   if        lin4 > 60
219800020619     c                   except    testa4
219900020619     c                   end
220000020619
220100020619     c                   if        lin3 > 60
220200020619     c                   except    testa3
220300020619     c                   end
220400020619
220500020619     c                   if        lin2 > 60
220600020619     c                   except    testa2
220700020619     c                   end
220800020619
220900020619     c                   endsr
221000050506      *-------------------------------------------------
221100050506     c     pulriep       begsr
221200050506      *-------------------------------------------------
221300050506     C                   z-add     62            lung
221400050506     C                   clear                   comman
221500050506     C                   movea     cmd(5)        comman           80
221600050506     C                   call      'QCMDEXC'                            20
221700050506     C                   parm                    comman
221800050506     C                   parm                    lung
221900050506     c                   move      'X'           schedulato
222000050506     c                   open      fifsr00f
222100050506     c                   endsr
222200020521      *-------------------------------------------------
222300020521     c     *inzsr        begsr
222400020521      *-------------------------------------------------
222500020612     c     *entry        plist
222600020612     c                   parm                    kpjba
222700030225     c                   if        kpjbu = *blank
222800030225     c                   clear                   param
222900050506      * in caso di lancio schedulato pulisco il file riepiloghi
223000050506     c                   exsr      pulriep
223100030225     c                   else
223200030225     c                   movel     kpjbu         param
223300051123      * pulizia file riepiloghi x lancio extra fine mese sab/dom
223400051123     c                   if        riepcdg= 'S'
223500051123     c                   exsr      pulriep
223600051123     c                   endif
223700030225     c                   end
223800030226     c                   movea     dist          dista            10
223900030225      * se non trovo i parametri impostati sono in schedulazione quindi
224000050506      * ricavo i parametri per l'elaborazione a meno che non ci sia
224100050506      * la forzatura dell'udate x simulare la schedulazione forzata
224200050506
224300030227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
224400030227     C***
224500030227     C                   TIME                    W0120            14 0
224600030227     C                   MOVEl     W0120         Wora              6 0
224700030227     C                   MOVE      W0120         WDAT              8 0
224800050506      * verifico che non sia forzata la udate per la rigenerazione
224900050506      * del file riepiloghi per il controllo di gestione se la data
225000050506      * è valorizzata il lancio è forzato e non schedulato
225100050506     c                   if        frigen = 'S'
225200050506     c                   exsr      pulriep
225300050506     c                   z-add     fudate        wdat
225400050506     c                   endif
225500030225     c                   if        alladat = 0
225600030225     C*
225700030225     C                   Z-ADD     WDAT          G02DAT
225800030225     C                   MOVEL     *BLANK        G02ERR
225900030225     C                   CALL      'XSRDA8'
226000030225     C                   PARM                    WLBDAT
226100030225     C* UDATE A 8 IN AAAA/MM/GG
226200030225     C                   move      G02INV        dataiso
226300030225      *controllo se udate è festivo non lancio l'elaborazione
226400030225     c                   move      dataiso       dadata
226500030225     c                   move      dataiso       adata
226600030225     c                   CALL      'XSRLAV8'
226700030225     c                   PARM                    Wdata8
226800030225     c                   if        giolav = 0
226900051124      * RICHIAMO PGM PER INVIO EVENTUALE MAIL SE FINE MESE SAB/DOM
227000051124      * AVVISA CON MAIL DI LANCIARE MANUALMENTE IL C.E. GIORNALIERO
227100051124     c                   call      'FICN34R1'
227101130730     c                   parm                    kpjba
227200030225     c                   seton                                        lr
227300030225     c                   endif
227400030225      *reperisco limiti da tabella per calcolare il D-Day
227500030225     c                   movel(p)  '1'           tbeke1
227600030225     c                   movel     'CCG'         tbecod
227700030225     c     ktbe          chain     tntbe01l                           90
227800030225     c                   if        not *in90
227900030225     c                   movel     tbeuni        dccg
228000030225     c                   endif
228100030225      * calcolo il D-Day partendo da udate cercando i giorni lavorativi
228200030225      * all'indietro a seconda dei giorni impostati in tabella
228300030225     c     dataiso       subdur    1:*d          dataiso2
228400030225     c                   do        *hival
228500030225     c                   move      dataiso2      dadata
228600030225     c                   move      dataiso       adata
228700030225     c                   CALL      'XSRLAV8'
228800030225     c                   PARM                    Wdata8
228900030225      * se corrisponde ho trovato il D-Day
229000030225     c                   if        giolav = §cggce
229100030225     c                   leave
229200030225     c                   else
229300030225      * altrimenti continuo a sottrarre i giorni da udate
229400030225     c     dataiso2      subdur    1:*d          dataiso2
229500030225     c                   iter
229600030225     c                   endif
229700030225     c                   enddo
229800030225      * ho stabilito il D-DAY
229900030225     c                   move      dataiso2      alladat
230000030225     c                   move      alladat       dalladat
230100030225     c                   move      01            dalladat
230200030225
230300030225     c                   else
230400030225
230500030225      *lancio da video parto da qui con data fine periodo già impostata
230600030225     c                   move      alladat       dataiso2
230700030225     c                   endif
230800030225      * verifico se i giorni precedenti al D-Day sono festivi per accorparli
230900030225     c     dataiso2      subdur    1:*d          dataiso3
231000030225     c                   do        *hival
231100030225     c                   move      dataiso3      dadata
231200030225     c                   move      dataiso2      adata
231300030225     c                   CALL      'XSRLAV8'
231400030225     c                   PARM                    Wdata8
231500030225      * quando i giorni lavorativi diventano 2 vuol dire che ho chiuso il
231600030225      * sottoperiodo di raggruppamento del D-DAY
231700030225      * esempio: se il D-DAY è lunedi il sabato e domenica precedenti
231800030225      * vanno sommati assieme al D-Day nella stampa
231900051026      * NEW- questo avviene solo se non ci troviamo a cavallo di mese
232000051026      * nell'esempio sopra riportato
232100051026      *__________ verifico se il mese delle date in esame è uguale _______
232200051026     c                   extrct    dataiso2:*m   mese1             2 0
232300051026     c                   extrct    dataiso3:*m   mese2             2 0
232400030225
232500030225      * se trovo il secondo giorno lavorativo aggiungo uno per tornare al
232600030225      * giorno seguente (calendario) che è la data di partenza del periodo
232700030225      * del D-day
232800051026     c                   if        giolav = 2 or mese1 <> mese2
232900030225     c     dataiso3      adddur    1:*d          dataiso3
233000030225     c                   leave
233100030225     c                   else
233200030225      * altrimenti continuo a sottrarre i giorni da udate
233300030225     c     dataiso3      subdur    1:*d          dataiso3
233400030225     c                   iter
233500030225     c                   endif
233600030225     c                   enddo
233700030225      * ho stabilito eventuale ragguppamento del D-DAY
233800030225     c                   move      dataiso3      alladatx
233900020612
234000020611     c                   z-add     66            lin1
234100020611     c                   z-add     66            lin2
234200020611     c                   z-add     66            lin3
234300020619     c                   z-add     66            lin4
234400020619     c                   z-add     3             livello
234500030508      *ovr per ecoprt4 report globale di sede
234600030508     C                   z-add     62            lung
234700030508     C                   clear                   comman
234800030508     C                   movea     cmd(4)        comman           80
234900030603     c                   if        dOvr = 'N'
235000030603     C                   EVAL      %SUBST(comman:52:10) = OutqSede
235100030603     c                   end
235200030508     C                   call      'QCMDEXC'                            20
235300030508     C                   parm                    comman
235400030508     C                   parm                    lung
235500030508     c                   open      ecoprt4
235600030508      *
235700020611     c     kfss          klist
235800020611     c                   kfld                    orgfil
235900020611     c                   kfld                    dalladat
236000030225
236100020619     c     ktab          klist
236200020619     c                   kfld                    tblkut
236300020619     c                   kfld                    tblcod
236400020619     c                   kfld                    tblkey
236500020619     c                   z-add     1             tblkut
236600020619      *
236700030225     c     ktbe          klist
236800030225     c                   kfld                    tbecod
236900030225     c                   kfld                    tbeke1
237000030225      *
237100030515     c     kfre          klist
237200030515     c                   kfld                    kfretip           1
237300030515     c                   kfld                    kfrepdr           7 0
237400030515     c                   kfld                    kfreddc           8 0
237500030515     c                   kfld                    kfretsr           1
237600030522      *
237700020619     c                   move      alladat       dataiso
237800020619     c                   move      dataiso       dataeur
237900020619     c                   move      dataeur       alladatg
238000030225
238100020619     c                   move      dalladat      dataiso
238200020619     c                   move      dataiso       dataeur
238300020619     c                   move      dataeur       dalladatg
238400020521
238500020521     c                   endsr
238600020517     O*------------------------------------------------------*
238700020619     Oecoprt1   e            TESTA1            2
238800020625     o                                            8 'FICN34R'
238900020625     o                       *date         y   +  3
239000030227     o                       wora              +  2 '  :  :  '
239100030319     O                                          100 'Stampa C.E. giornaliero -
239200030314     O                                              Costi variabili'
239300030319     o                                          110 'Dal '
239400020619     o                       dalladatg         +  2 '  /  /    '
239500020620     o                                         +  2 'Al '
239600020619     o                       alladatg          +  2 '  /  /    '
239700030314     o                                         +  2 'Per Punto Operativo'
239800030314     O          e            TESTA1      0
239900030319     O                                          100 'Stampa C.E. giornaliero -
240000030314     O                                              Costi variabili'
240100030319     o                                          110 'Dal '
240200020625     o                       dalladatg         +  2 '  /  /    '
240300020625     o                                         +  2 'Al '
240400020625     o                       alladatg          +  2 '  /  /    '
240500020625     o                                         +  2 'Per Punto Operativo'
240600020625     O          e            TESTA1      0
240700030319     O                                          100 'Stampa C.E. giornaliero -
240800030314     O                                              Costi variabili'
240900030319     o                                          110 'Dal '
241000020625     o                       dalladatg         +  2 '  /  /    '
241100020625     o                                         +  2 'Al '
241200020625     o                       alladatg          +  2 '  /  /    '
241300020625     o                                         +  2 'Per Punto Operativo'
241400020619     O          e            testa1      2
241500020620     O                                           12 'P.Operativo'
241600020620     O                                           80 'Giornata del'
241700020625     o                       alladatg            95 '  /  /    '
241800020620     O                                          150 'Progressivo al'
241900020620     o                       alladatg           165 '  /  /    '
242000020625     O          e            testa1      0
242100020625     O                                           12 'P.Operativo'
242200020625     O                                           80 'Giornata del'
242300020625     o                       alladatg            95 '  /  /    '
242400020625     O                                          150 'Progressivo al'
242500020625     o                       alladatg           165 '  /  /    '
242600020625     O          e            testa1      0
242700020625     O                                           12 'P.Operativo'
242800020625     O                                           80 'Giornata del'
242900020625     o                       alladatg            95 '  /  /    '
243000020625     O                                          150 'Progressivo al'
243100020625     o                       alladatg           165 '  /  /    '
243200020619     O          e            testa1      1
243300060913     o                                           47 'C.Stimati'
243400030528     o                                           60 'Costi'
243500030528     o                                           74 'Costi'
243600030528     o                                           88 'Competenze'
243700030528     o                                          110 'Scostamento'
243800020619      *
243900060913     o                                          130 'C.Stimati'
244000030528     o                                          142 'Costi'
244100030528     o                                          156 'Costi'
244200030528     o                                          170 'Competenze'
244300030528     o                                          192 'Scostamento'
244400020625     O          e            testa1      0
244500060913     o                                           47 'C.Stimati'
244600030528     o                                           60 'Costi'
244700030528     o                                           74 'Costi'
244800030528     o                                           88 'Competenze'
244900030528     o                                          110 'Scostamento'
245000030528      *
245100060913     o                                          130 'C.Stimati'
245200030528     o                                          142 'Costi'
245300030528     o                                          156 'Costi'
245400030528     o                                          170 'Competenze'
245500030528     o                                          192 'Scostamento'
245600020625     O          e            testa1      0
245700060913     o                                           47 'C.Stimati'
245800030528     o                                           60 'Costi'
245900030528     o                                           74 'Costi'
246000030528     o                                           88 'Competenze'
246100030528     o                                          110 'Scostamento'
246200030528      *
246300060913     o                                          130 'C.Stimati'
246400030528     o                                          142 'Costi'
246500030528     o                                          156 'Costi'
246600030528     o                                          170 'Competenze'
246700030528     o                                          192 'Scostamento'
246800020619     O          e            testa1      1
246900060913     o                                           48 'Rettifiche'
247000030528     o                                           60 'Reali'
247100030528     o                                           74 'Totali'
247200030528     o                                          104 'Valore'
247300030203     o                                          116 'Delta %'
247400020619      *
247500060913     o                                          130 'Rettifiche'
247600030528     o                                          142 'Reali'
247700030528     o                                          156 'Totali'
247800030528     o                                          186 'Valore'
247900030522     o                                          198 'Delta %'
248000020625     O          e            testa1      0
248100060913     o                                           48 'Rettifiche'
248200030528     o                                           60 'Reali'
248300030528     o                                           74 'Totali'
248400030528     o                                          104 'Valore'
248500030528     o                                          116 'Delta %'
248600030528      *
248700060913     o                                          130 'Rettifiche'
248800030528     o                                          142 'Reali'
248900030528     o                                          156 'Totali'
249000030528     o                                          186 'Valore'
249100030528     o                                          198 'Delta %'
249200020625     O          e            testa1      0
249300060913     o                                           48 'Rettifiche'
249400030528     o                                           60 'Reali'
249500030528     o                                           74 'Totali'
249600030528     o                                          104 'Valore'
249700030528     o                                          116 'Delta %'
249800030528      *
249900060913     o                                          130 'Rettifiche'
250000030528     o                                          142 'Reali'
250100030528     o                                          156 'Totali'
250200030528     o                                          186 'Valore'
250300030528     o                                          198 'Delta %'
250400020619     O          e            detta       1
250500020620     O                       exfil                3
250600020620     o                       desfil              20
250700030527     o                                           30 'Aut.'
250800060913     o                       totstm1ds     j     46
250900030528     o                       totcos1ds     2     60
251000060913     o                       totsco1s      j     74
251100030528     o                       totcom1ds     2     88
251200030528     o                       totdif1s      j    104
251300030528     o                       totper1s      j    116
251400030528      *
251500060913     o                       totstm1ps     j    128
251600030528     o                       totcos1ps     2    142
251700060913     o                       totsco1ps     j    156
251800030528     o                       totcom1ps     2    170
251900030528     o                       totdif1ps     j    186
252000030528     o                       totper1ps     j    198
252100030115     O          e            detta       1
252200030527     o                                           30 'Coop'
252300060913     o                       totstm1dsx    j     46
252400030528     o                       totcos1dsx    2     60
252500060913     o                       totsco1sx     j     74
252600030528     o                       totcom1dsx    2     88
252700030528     o                       totdif1sx     j    104
252800030528     o                       totper1sx     j    116
252900030528     o
253000060913     o                       totstm1psx    j    128
253100030528     o                       totcos1psx    2    142
253200060913     o                       totsco1psx    j    156
253300030528     o                       totcom1psx    2    170
253400030528     o                       totdif1psx    j    186
253500030528     o                       totper1psx    j    198
253600030515     O          e            detta       1
253700030527     o                                           30 'AfDe'
253800060913     o                       totstm1dsa    j     46
253900030528     o                       totcos1dsa    2     60
254000060913     o                       totsco1sa     j     74
254100030528     o                       totcom1dsa    2     88
254200030528     o                       totdif1sa     j    104
254300030528     o                       totper1sa     j    116
254400030528     o
254500060913     o                       totstm1psa    j    128
254600030528     o                       totcos1psa    2    142
254700060913     o                       totsco1psa    j    156
254800030528     o                       totcom1psa    2    170
254900030528     o                       totdif1psa    j    186
255000030528     o                       totper1psa    j    198
255100030528     O          e            detta       1
255200030528     o                                           30 'Tot.'
255300060913     o                       stime1        j     46
255400030528     o                       reali1        2     60
255500060913     o                       costi1        j     74
255600030528     o                       compe1        2     88
255700030528     o                       diffe1        j    104
255800030528     o                       tpcompe1      j    116
255900030528     o
256000060913     o                       stime1p       j    128
256100030528     o                       reali1p       2    142
256200060913     o                       costi1p       j    156
256300030528     o                       compe1p       2    170
256400030528     o                       diffe1p       j    186
256500030528     o                       tpcompe1p     j    198
256600030528     O          e            detta       0
256700030528     o                                           30 'Tot.'
256800060913     o                       stime1        j     46
256900030528     o                       reali1        2     60
257000060913     o                       costi1        j     74
257100030528     o                       compe1        2     88
257200030528     o                       diffe1        j    104
257300030528     o                       tpcompe1      j    116
257400030528     o
257500060913     o                       stime1p       j    128
257600030528     o                       reali1p       2    142
257700060913     o                       costi1p       j    156
257800030528     o                       compe1p       2    170
257900030528     o                       diffe1p       j    186
258000030528     o                       tpcompe1p     j    198
258100030528     O          e            detta       0
258200030528     o                                           30 'Tot.'
258300060913     o                       stime1        j     46
258400030528     o                       reali1        2     60
258500060913     o                       costi1        j     74
258600030528     o                       compe1        2     88
258700030528     o                       diffe1        j    104
258800030528     o                       tpcompe1      j    116
258900030528     o
259000060913     o                       stime1p       j    128
259100030528     o                       reali1p       2    142
259200060913     o                       costi1p       j    156
259300030528     o                       compe1p       2    170
259400030528     o                       diffe1p       j    186
259500030528     o                       tpcompe1p     j    198
259600020619     Oecoprt2   e            TESTA2            2
259700020625     o                                            8 'FICN34R'
259800020625     o                       *date         y   +  3
259900030227     o                       wora              +  2 '  :  :  '
260000030319     O                                          100 'Stampa C.E. giornaliero -
260100030314     O                                              Costi variabili'
260200030319     o                                          110 'Dal '
260300020619     o                       dalladatg         +  2 '  /  /    '
260400020620     o                                         +  2 'Al '
260500020619     o                       alladatg          +  2 '  /  /    '
260600020625     o                                         +  2 'Per Area'
260700020625     O          e            TESTA2      0
260800030319     O                                          100 'Stampa C.E. giornaliero -
260900030314     O                                              Costi variabili'
261000030319     o                                          110 'Dal '
261100020625     o                       dalladatg         +  2 '  /  /    '
261200020625     o                                         +  2 'Al '
261300020625     o                       alladatg          +  2 '  /  /    '
261400020625     o                                         +  2 'Per Area'
261500020625     O          e            TESTA2      0
261600030319     O                                          100 'Stampa C.E. giornaliero -
261700030314     O                                              Costi variabili'
261800030319     o                                          110 'Dal '
261900020625     o                       dalladatg         +  2 '  /  /    '
262000020625     o                                         +  2 'Al '
262100020625     o                       alladatg          +  2 '  /  /    '
262200020625     o                                         +  2 'Per Area'
262300020619     O          e            testa2      2
262400020620     O                                           12 'P.Operativo'
262500020620     O                                           80 'Giornata del'
262600020620     o                       alladatg            95 '  /  /    '
262700020620     O                                          150 'Progressivo al'
262800020620     o                       alladatg           165 '  /  /    '
262900020625     O          e            testa2      0
263000020625     O                                           12 'P.Operativo'
263100020625     O                                           80 'Giornata del'
263200020625     o                       alladatg            95 '  /  /    '
263300020625     O                                          150 'Progressivo al'
263400020625     o                       alladatg           165 '  /  /    '
263500020625     O          e            testa2      0
263600020625     O                                           12 'P.Operativo'
263700020625     O                                           80 'Giornata del'
263800020625     o                       alladatg            95 '  /  /    '
263900020625     O                                          150 'Progressivo al'
264000020625     o                       alladatg           165 '  /  /    '
264100020619     O          e            testa2      1
264200060913     o                                           47 'C.Stimati'
264300030528     o                                           60 'Costi'
264400030528     o                                           74 'Costi'
264500030528     o                                           88 'Competenze'
264600030528     o                                          110 'Scostamento'
264700030528      *
264800060913     o                                          130 'C.Stimati'
264900030528     o                                          142 'Costi'
265000030528     o                                          156 'Costi'
265100030528     o                                          170 'Competenze'
265200030528     o                                          192 'Scostamento'
265300020625     O          e            testa2      0
265400060913     o                                           47 'C.Stimati'
265500030528     o                                           60 'Costi'
265600030528     o                                           74 'Costi'
265700030528     o                                           88 'Competenze'
265800030528     o                                          110 'Scostamento'
265900030528      *
266000060913     o                                          130 'C.Stimati'
266100030528     o                                          142 'Costi'
266200030528     o                                          156 'Costi'
266300030528     o                                          170 'Competenze'
266400030528     o                                          192 'Scostamento'
266500020625     O          e            testa2      0
266600060913     o                                           47 'C.Stimati'
266700030528     o                                           60 'Costi'
266800030528     o                                           74 'Costi'
266900030528     o                                           88 'Competenze'
267000030528     o                                          110 'Scostamento'
267100030528      *
267200060913     o                                          130 'C.Stimati'
267300030528     o                                          142 'Costi'
267400030528     o                                          156 'Costi'
267500030528     o                                          170 'Competenze'
267600030528     o                                          192 'Scostamento'
267700020619     O          e            testa2      1
267800060913     o                                           48 'Rettifiche'
267900030528     o                                           60 'Reali'
268000030528     o                                           74 'Totali'
268100030528     o                                          104 'Valore'
268200030528     o                                          116 'Delta %'
268300030528      *
268400060913     o                                          130 'Rettifiche'
268500030528     o                                          142 'Reali'
268600030528     o                                          156 'Totali'
268700030528     o                                          186 'Valore'
268800030528     o                                          198 'Delta %'
268900020625     O          e            testa2      0
269000060913     o                                           48 'Rettifiche'
269100030528     o                                           60 'Reali'
269200030528     o                                           74 'Totali'
269300030528     o                                          104 'Valore'
269400030528     o                                          116 'Delta %'
269500030528      *
269600060913     o                                          130 'Rettifiche'
269700030528     o                                          142 'Reali'
269800030528     o                                          156 'Totali'
269900030528     o                                          186 'Valore'
270000030528     o                                          198 'Delta %'
270100020625     O          e            testa2      0
270200060913     o                                           48 'Rettifiche'
270300030528     o                                           60 'Reali'
270400030528     o                                           74 'Totali'
270500030528     o                                          104 'Valore'
270600030528     o                                          116 'Delta %'
270700030528      *
270800060913     o                                          130 'Rettifiche'
270900030528     o                                          142 'Reali'
271000030528     o                                          156 'Totali'
271100030528     o                                          186 'Valore'
271200030528     o                                          198 'Delta %'
271300020619     O          e            detta       1
271400020620     O                       exfil                3
271500020620     o                       desfil              20
271600030527     o                                           30 'Aut.'
271700060913     o                       totstm1ds     j     46
271800030528     o                       totcos1ds     2     60
271900060913     o                       totsco1s      j     74
272000030528     o                       totcom1ds     2     88
272100030528     o                       totdif1s      j    104
272200030528     o                       totper1s      j    116
272300030528     o
272400060913     o                       totstm1ps     j    128
272500030528     o                       totcos1ps     2    142
272600060913     o                       totsco1ps     j    156
272700030528     o                       totcom1ps     2    170
272800030528     o                       totdif1ps     j    186
272900030528     o                       totper1ps     j    198
273000030115     O          e            detta       1
273100030527     o                                           30 'Coop'
273200060913     o                       totstm1dsx    j     46
273300030528     o                       totcos1dsx    2     60
273400060913     o                       totsco1sx     j     74
273500030528     o                       totcom1dsx    2     88
273600030528     o                       totdif1sx     j    104
273700030528     o                       totper1sx     j    116
273800030528     o
273900060913     o                       totstm1psx    j    128
274000030528     o                       totcos1psx    2    142
274100060913     o                       totsco1psx    j    156
274200030528     o                       totcom1psx    2    170
274300030528     o                       totdif1psx    j    186
274400030528     o                       totper1psx    j    198
274500030515     O          e            detta       1
274600030527     o                                           30 'AfDe'
274700060913     o                       totstm1dsa    j     46
274800030528     o                       totcos1dsa    2     60
274900060913     o                       totsco1sa     j     74
275000030528     o                       totcom1dsa    2     88
275100030528     o                       totdif1sa     j    104
275200030528     o                       totper1sa     j    116
275300030528     o
275400060913     o                       totstm1psa    j    128
275500030528     o                       totcos1psa    2    142
275600060913     o                       totsco1psa    j    156
275700030528     o                       totcom1psa    2    170
275800030528     o                       totdif1psa    j    186
275900030528     o                       totper1psa    j    198
276000030528     O          e            detta       1
276100030528     o                                           30 'Tot.'
276200060913     o                       stime1        j     46
276300030528     o                       reali1        2     60
276400060913     o                       costi1        j     74
276500030528     o                       compe1        2     88
276600030528     o                       diffe1        j    104
276700030528     o                       tpcompe1      j    116
276800030528     o
276900060913     o                       stime1p       j    128
277000030528     o                       reali1p       2    142
277100060913     o                       costi1p       j    156
277200030528     o                       compe1p       2    170
277300030528     o                       diffe1p       j    186
277400030528     o                       tpcompe1p     j    198
277500030528     O          e            detta       0
277600030528     o                                           30 'Tot.'
277700060913     o                       stime1        j     46
277800030528     o                       reali1        2     60
277900060913     o                       costi1        j     74
278000030528     o                       compe1        2     88
278100030528     o                       diffe1        j    104
278200030528     o                       tpcompe1      j    116
278300030528     o
278400060913     o                       stime1p       j    128
278500030528     o                       reali1p       2    142
278600060913     o                       costi1p       j    156
278700030528     o                       compe1p       2    170
278800030528     o                       diffe1p       j    186
278900030528     o                       tpcompe1p     j    198
279000030528     O          e            detta       0
279100030528     o                                           30 'Tot.'
279200060913     o                       stime1        j     46
279300030528     o                       reali1        2     60
279400060913     o                       costi1        j     74
279500030528     o                       compe1        2     88
279600030528     o                       diffe1        j    104
279700030528     o                       tpcompe1      j    116
279800030528     o
279900060913     o                       stime1p       j    128
280000030528     o                       reali1p       2    142
280100060913     o                       costi1p       j    156
280200030528     o                       compe1p       2    170
280300030528     o                       diffe1p       j    186
280400030528     o                       tpcompe1p     j    198
280500030528     O          e            area        1
280600020619     O                                           12 'Totale area'
280700020619     o                       exarea              16
280800030528     o                                           30 'Aut.'
280900060913     o                       totstm2ds     j     46
281000030528     o                       totcos2ds     2     60
281100060913     o                       totsco2s      j     74
281200030528     o                       totcom2ds     2     88
281300030528     o                       totdif2s      j    104
281400030528     o                       totper2s      j    116
281500030528     o
281600060913     o                       totstm2ps     j    128
281700030528     o                       totcos2ps     2    142
281800060913     o                       totsco2ps     j    156
281900030528     o                       totcom2ps     2    170
282000030528     o                       totdif2ps     j    186
282100030528     o                       totper2ps     j    198
282200030527     O          e            area        1
282300030528     o                       exdesa              20
282400030528     o                                           30 'Coop'
282500060913     o                       totstm2dsx    j     46
282600030528     o                       totcos2dsx    2     60
282700060913     o                       totsco2sx     j     74
282800030528     o                       totcom2dsx    2     88
282900030528     o                       totdif2sx     j    104
283000030528     o                       totper2sx     j    116
283100030528     o
283200060913     o                       totstm2psx    j    128
283300030528     o                       totcos2psx    2    142
283400060913     o                       totsco2psx    j    156
283500030528     o                       totcom2psx    2    170
283600030528     o                       totdif2psx    j    186
283700030528     o                       totper2psx    j    198
283800030528     O          e            area        1
283900030528     o                                           30 'AfDe'
284000060913     o                       totstm2dsa    j     46
284100030528     o                       totcos2dsa    2     60
284200060913     o                       totsco2sa     j     74
284300030528     o                       totcom2dsa    2     88
284400030528     o                       totdif2sa     j    104
284500030528     o                       totper2sa     j    116
284600030528     o
284700060913     o                       totstm2psa    j    128
284800030528     o                       totcos2psa    2    142
284900060913     o                       totsco2psa    j    156
285000030528     o                       totcom2psa    2    170
285100030528     o                       totdif2psa    j    186
285200030528     o                       totper2psa    j    198
285300030528     O          e            area        1
285400030528     o                                           30 'Tot.'
285500060913     o                       stime2        j     46
285600030528     o                       reali2        2     60
285700060913     o                       costi2        j     74
285800030528     o                       compe2        2     88
285900030528     o                       diffe2        j    104
286000030528     o                       tpcompe2      j    116
286100030528     o
286200060913     o                       stime2p       j    128
286300030528     o                       reali2p       2    142
286400060913     o                       costi2p       j    156
286500030528     o                       compe2p       2    170
286600030528     o                       diffe2p       j    186
286700030528     o                       tpcompe2p     j    198
286800030528     O          e            area        0
286900030528     o                                           30 'Tot.'
287000060913     o                       stime2        j     46
287100030528     o                       reali2        2     60
287200060913     o                       costi2        j     74
287300030528     o                       compe2        2     88
287400030528     o                       diffe2        j    104
287500030528     o                       tpcompe2      j    116
287600030528     o
287700060913     o                       stime2p       j    128
287800030528     o                       reali2p       2    142
287900060913     o                       costi2p       j    156
288000030528     o                       compe2p       2    170
288100030528     o                       diffe2p       j    186
288200030528     o                       tpcompe2p     j    198
288300030528     O          e            area        0  1
288400030528     o                                           30 'Tot.'
288500060913     o                       stime2        j     46
288600030528     o                       reali2        2     60
288700060913     o                       costi2        j     74
288800030528     o                       compe2        2     88
288900030528     o                       diffe2        j    104
289000030528     o                       tpcompe2      j    116
289100030528     o
289200060913     o                       stime2p       j    128
289300030528     o                       reali2p       2    142
289400060913     o                       costi2p       j    156
289500030528     o                       compe2p       2    170
289600030528     o                       diffe2p       j    186
289700030528     o                       tpcompe2p     j    198
289800020619     Oecoprt3   e            TESTA3            2
289900020625     o                                            8 'FICN34R'
290000020625     o                       *date         y   +  3
290100030227     o                       wora              +  2 '  :  :  '
290200030319     O                                          100 'Stampa C.E. giornaliero -
290300030314     O                                              Costi variabili'
290400030319     o                                          110 'Dal '
290500020619     o                       dalladatg         +  2 '  /  /    '
290600020620     o                                         +  2 'Al '
290700020619     o                       alladatg          +  2 '  /  /    '
290800100204     o                                         +  2 'Per Dir.Comm.'
290900020625     O          e            TESTA3      0
291000030319     O                                          100 'Stampa C.E. giornaliero -
291100030314     O                                              Costi variabili'
291200030319     o                                          110 'Dal '
291300020625     o                       dalladatg         +  2 '  /  /    '
291400020625     o                                         +  2 'Al '
291500020625     o                       alladatg          +  2 '  /  /    '
291600100204     o                                         +  2 'Per Dir.Comm.'
291700020625     O          e            TESTA3      0
291800030319     O                                          100 'Stampa C.E. giornaliero -
291900030314     O                                              Costi variabili'
292000030319     o                                          110 'Dal '
292100020625     o                       dalladatg         +  2 '  /  /    '
292200020625     o                                         +  2 'Al '
292300020625     o                       alladatg          +  2 '  /  /    '
292400100204     o                                         +  2 'Per Dir.Comm.'
292500020619     O          e            testa3      2
292600020620     O                                           12 'P.Operativo'
292700020620     O                                           80 'Giornata del'
292800020620     o                       alladatg            95 '  /  /    '
292900020620     O                                          150 'Progressivo al'
293000020620     o                       alladatg           165 '  /  /    '
293100020625     O          e            testa3      0
293200020625     O                                           12 'P.Operativo'
293300020625     O                                           80 'Giornata del'
293400020625     o                       alladatg            95 '  /  /    '
293500020625     O                                          150 'Progressivo al'
293600020625     o                       alladatg           165 '  /  /    '
293700020625     O          e            testa3      0
293800020625     O                                           12 'P.Operativo'
293900020625     O                                           80 'Giornata del'
294000020625     o                       alladatg            95 '  /  /    '
294100020625     O                                          150 'Progressivo al'
294200020625     o                       alladatg           165 '  /  /    '
294300020619     O          e            testa3      1
294400060913     o                                           47 'C.Stimati'
294500030528     o                                           60 'Costi'
294600030528     o                                           74 'Costi'
294700030528     o                                           88 'Competenze'
294800030528     o                                          110 'Scostamento'
294900030528      *
295000060913     o                                          130 'C.Stimati'
295100030528     o                                          142 'Costi'
295200030528     o                                          156 'Costi'
295300030528     o                                          170 'Competenze'
295400030528     o                                          192 'Scostamento'
295500020625     O          e            testa3      0
295600060913     o                                           47 'C.Stimati'
295700030528     o                                           60 'Costi'
295800030528     o                                           74 'Costi'
295900030528     o                                           88 'Competenze'
296000030528     o                                          110 'Scostamento'
296100030528      *
296200060913     o                                          130 'C.Stimati'
296300030528     o                                          142 'Costi'
296400030528     o                                          156 'Costi'
296500030528     o                                          170 'Competenze'
296600030528     o                                          192 'Scostamento'
296700020625     O          e            testa3      0
296800060913     o                                           47 'C.Stimati'
296900030528     o                                           60 'Costi'
297000030528     o                                           74 'Costi'
297100030528     o                                           88 'Competenze'
297200030528     o                                          110 'Scostamento'
297300030528      *
297400060913     o                                          130 'C.Stimati'
297500030528     o                                          142 'Costi'
297600030528     o                                          156 'Costi'
297700030528     o                                          170 'Competenze'
297800030528     o                                          192 'Scostamento'
297900020619     O          e            testa3      1
298000060913     o                                           48 'Rettifiche'
298100030528     o                                           60 'Reali'
298200030528     o                                           74 'Totali'
298300030528     o                                          104 'Valore'
298400030528     o                                          116 'Delta %'
298500030528      *
298600060913     o                                          130 'Rettifiche'
298700030528     o                                          142 'Reali'
298800030528     o                                          156 'Totali'
298900030528     o                                          186 'Valore'
299000030528     o                                          198 'Delta %'
299100020625     O          e            testa3      0
299200060913     o                                           48 'Rettifiche'
299300030528     o                                           60 'Reali'
299400030528     o                                           74 'Totali'
299500030528     o                                          104 'Valore'
299600030528     o                                          116 'Delta %'
299700030528      *
299800060913     o                                          130 'Rettifiche'
299900030528     o                                          142 'Reali'
300000030528     o                                          156 'Totali'
300100030528     o                                          186 'Valore'
300200030528     o                                          198 'Delta %'
300300030528     O          e            testa3      0
300400060913     o                                           48 'Rettifiche'
300500030528     o                                           60 'Reali'
300600030528     o                                           74 'Totali'
300700030528     o                                          104 'Valore'
300800030528     o                                          116 'Delta %'
300900030528      *
301000060913     o                                          130 'Rettifiche'
301100030528     o                                          142 'Reali'
301200030528     o                                          156 'Totali'
301300030528     o                                          186 'Valore'
301400030528     o                                          198 'Delta %'
301500020619     O          e            detta       1
301600020620     O                       exfil                3
301700020620     o                       desfil              20
301800030527     o                                           30 'Aut.'
301900060913     o                       totstm1ds     j     46
302000030528     o                       totcos1ds     2     60
302100060913     o                       totsco1s      j     74
302200030528     o                       totcom1ds     2     88
302300030528     o                       totdif1s      j    104
302400030528     o                       totper1s      j    116
302500030528     o
302600060913     o                       totstm1ps     j    128
302700030528     o                       totcos1ps     2    142
302800060913     o                       totsco1ps     j    156
302900030528     o                       totcom1ps     2    170
303000030528     o                       totdif1ps     j    186
303100030528     o                       totper1ps     j    198
303200030115     O          e            detta       1
303300030527     o                                           30 'Coop'
303400060913     o                       totstm1dsx    j     46
303500030528     o                       totcos1dsx    2     60
303600060913     o                       totsco1sx     j     74
303700030528     o                       totcom1dsx    2     88
303800030528     o                       totdif1sx     j    104
303900030528     o                       totper1sx     j    116
304000030528     o
304100060913     o                       totstm1psx    j    128
304200030528     o                       totcos1psx    2    142
304300060913     o                       totsco1psx    j    156
304400030528     o                       totcom1psx    2    170
304500030528     o                       totdif1psx    j    186
304600030528     o                       totper1psx    j    198
304700030515     O          e            detta       1
304800030527     o                                           30 'AfDe'
304900060913     o                       totstm1dsa    j     46
305000030528     o                       totcos1dsa    2     60
305100060913     o                       totsco1sa     j     74
305200030528     o                       totcom1dsa    2     88
305300030528     o                       totdif1sa     j    104
305400030528     o                       totper1sa     j    116
305500030528     o
305600060913     o                       totstm1psa    j    128
305700030528     o                       totcos1psa    2    142
305800060913     o                       totsco1psa    j    156
305900030528     o                       totcom1psa    2    170
306000030528     o                       totdif1psa    j    186
306100030528     o                       totper1psa    j    198
306200030528     O          e            detta       1
306300030528     o                                           30 'Tot.'
306400060913     o                       stime1        j     46
306500030528     o                       reali1        2     60
306600060913     o                       costi1        j     74
306700030528     o                       compe1        2     88
306800030528     o                       diffe1        j    104
306900030528     o                       tpcompe1      j    116
307000030528     o
307100060913     o                       stime1p       j    128
307200030528     o                       reali1p       2    142
307300060913     o                       costi1p       j    156
307400030528     o                       compe1p       2    170
307500030528     o                       diffe1p       j    186
307600030528     o                       tpcompe1p     j    198
307700030528     O          e            detta       0
307800030528     o                                           30 'Tot.'
307900060913     o                       stime1        j     46
308000030528     o                       reali1        2     60
308100060913     o                       costi1        j     74
308200030528     o                       compe1        2     88
308300030528     o                       diffe1        j    104
308400030528     o                       tpcompe1      j    116
308500030528     o
308600060913     o                       stime1p       j    128
308700030528     o                       reali1p       2    142
308800060913     o                       costi1p       j    156
308900030528     o                       compe1p       2    170
309000030528     o                       diffe1p       j    186
309100030528     o                       tpcompe1p     j    198
309200030528     O          e            detta       0
309300030528     o                                           30 'Tot.'
309400060913     o                       stime1        j     46
309500030528     o                       reali1        2     60
309600060913     o                       costi1        j     74
309700030528     o                       compe1        2     88
309800030528     o                       diffe1        j    104
309900030528     o                       tpcompe1      j    116
310000030528     o
310100060913     o                       stime1p       j    128
310200030528     o                       reali1p       2    142
310300060913     o                       costi1p       j    156
310400030528     o                       compe1p       2    170
310500030528     o                       diffe1p       j    186
310600030528     o                       tpcompe1p     j    198
310700030528     O          e            area        1
310800020619     O                                           12 'Totale area'
310900020619     o                       exarea              16
311000030528     o                                           30 'Aut.'
311100060913     o                       totstm2ds     j     46
311200030528     o                       totcos2ds     2     60
311300060913     o                       totsco2s      j     74
311400030528     o                       totcom2ds     2     88
311500030528     o                       totdif2s      j    104
311600030528     o                       totper2s      j    116
311700030528     o
311800060913     o                       totstm2ps     j    128
311900030528     o                       totcos2ps     2    142
312000060913     o                       totsco2ps     j    156
312100030528     o                       totcom2ps     2    170
312200030528     o                       totdif2ps     j    186
312300030528     o                       totper2ps     j    198
312400030527     O          e            area        1
312500030528     o                       exdesa              20
312600030528     o                                           30 'Coop'
312700060913     o                       totstm2dsx    j     46
312800030528     o                       totcos2dsx    2     60
312900060913     o                       totsco2sx     j     74
313000030528     o                       totcom2dsx    2     88
313100030528     o                       totdif2sx     j    104
313200030528     o                       totper2sx     j    116
313300030528     o
313400060913     o                       totstm2psx    j    128
313500030528     o                       totcos2psx    2    142
313600060913     o                       totsco2psx    j    156
313700030528     o                       totcom2psx    2    170
313800030528     o                       totdif2psx    j    186
313900030528     o                       totper2psx    j    198
314000030528     O          e            area        1
314100030528     o                                           30 'AfDe'
314200060913     o                       totstm2dsa    j     46
314300030528     o                       totcos2dsa    2     60
314400060913     o                       totsco2sa     j     74
314500030528     o                       totcom2dsa    2     88
314600030528     o                       totdif2sa     j    104
314700030528     o                       totper2sa     j    116
314800030528     o
314900060913     o                       totstm2psa    j    128
315000030528     o                       totcos2psa    2    142
315100060913     o                       totsco2psa    j    156
315200030528     o                       totcom2psa    2    170
315300030528     o                       totdif2psa    j    186
315400030528     o                       totper2psa    j    198
315500030528     O          e            area        1
315600030528     o                                           30 'Tot.'
315700060913     o                       stime2        j     46
315800030528     o                       reali2        2     60
315900060913     o                       costi2        j     74
316000030528     o                       compe2        2     88
316100030528     o                       diffe2        j    104
316200030528     o                       tpcompe2      j    116
316300030528     o
316400060913     o                       stime2p       j    128
316500030528     o                       reali2p       2    142
316600060913     o                       costi2p       j    156
316700030528     o                       compe2p       2    170
316800030528     o                       diffe2p       j    186
316900030528     o                       tpcompe2p     j    198
317000030528     O          e            area        0
317100030528     o                                           30 'Tot.'
317200060913     o                       stime2        j     46
317300030528     o                       reali2        2     60
317400060913     o                       costi2        j     74
317500030528     o                       compe2        2     88
317600030528     o                       diffe2        j    104
317700030528     o                       tpcompe2      j    116
317800030528     o
317900060913     o                       stime2p       j    128
318000030528     o                       reali2p       2    142
318100060913     o                       costi2p       j    156
318200030528     o                       compe2p       2    170
318300030528     o                       diffe2p       j    186
318400030528     o                       tpcompe2p     j    198
318500030528     O          e            area        0  1
318600030528     o                                           30 'Tot.'
318700060913     o                       stime2        j     46
318800030528     o                       reali2        2     60
318900060913     o                       costi2        j     74
319000030528     o                       compe2        2     88
319100030528     o                       diffe2        j    104
319200030528     o                       tpcompe2      j    116
319300030528     o
319400060913     o                       stime2p       j    128
319500030528     o                       reali2p       2    142
319600060913     o                       costi2p       j    156
319700030528     o                       compe2p       2    170
319800030528     o                       diffe2p       j    186
319900030528     o                       tpcompe2p     j    198
320000030528     O          e            distret     1
320100100204     O                                           18 'Totale Dir.Comm.'
320200020620     o                       exdistr             20
320300030528     o                                           30 'Aut.'
320400060913     o                       totstm3ds     j     46
320500030528     o                       totcos3ds     2     60
320600060913     o                       totsco3s      j     74
320700030528     o                       totcom3ds     2     88
320800030528     o                       totdif3s      j    104
320900030528     o                       totper3s      j    116
321000030528     o
321100060913     o                       totstm3ps     j    128
321200030528     o                       totcos3ps     2    142
321300060913     o                       totsco3ps     j    156
321400030528     o                       totcom3ps     2    170
321500030528     o                       totdif3ps     j    186
321600030528     o                       totper3ps     j    198
321700030527     O          e            distret     1
321800030528     o                       exdesd              20
321900030528     o                                           30 'Coop'
322000060913     o                       totstm3dsx    j     46
322100030528     o                       totcos3dsx    2     60
322200060913     o                       totsco3sx     j     74
322300030528     o                       totcom3dsx    2     88
322400030528     o                       totdif3sx     j    104
322500030528     o                       totper3sx     j    116
322600030528     o
322700060913     o                       totstm3psx    j    128
322800030528     o                       totcos3psx    2    142
322900060913     o                       totsco3psx    j    156
323000030528     o                       totcom3psx    2    170
323100030528     o                       totdif3psx    j    186
323200030528     o                       totper3psx    j    198
323300030528     O          e            distret     1
323400030528     o                                           30 'AfDe'
323500060913     o                       totstm3dsa    j     46
323600030528     o                       totcos3dsa    2     60
323700060913     o                       totsco3sa     j     74
323800030528     o                       totcom3dsa    2     88
323900030528     o                       totdif3sa     j    104
324000030528     o                       totper3sa     j    116
324100030528     o
324200060913     o                       totstm3psa    j    128
324300030528     o                       totcos3psa    2    142
324400060913     o                       totsco3psa    j    156
324500030528     o                       totcom3psa    2    170
324600030528     o                       totdif3psa    j    186
324700030528     o                       totper3psa    j    198
324800030528     O          e            distret     1
324900030528     o                                           30 'Tot.'
325000060913     o                       stime3        j     46
325100030528     o                       reali3        2     60
325200060913     o                       costi3        j     74
325300030528     o                       compe3        2     88
325400030528     o                       diffe3        j    104
325500030528     o                       tpcompe3      j    116
325600030528     o
325700060913     o                       stime3p       j    128
325800030528     o                       reali3p       2    142
325900060913     o                       costi3p       j    156
326000030528     o                       compe3p       2    170
326100030528     o                       diffe3p       j    186
326200030528     o                       tpcompe3p     j    198
326300030528     O          e            distret     0
326400030528     o                                           30 'Tot.'
326500060913     o                       stime3        j     46
326600030528     o                       reali3        2     60
326700060913     o                       costi3        j     74
326800060913     o                       compe3        2     88
326900030528     o                       diffe3        j    104
327000030528     o                       tpcompe3      j    116
327100030528     o
327200060913     o                       stime3p       j    128
327300030528     o                       reali3p       2    142
327400060913     o                       costi3p       j    156
327500030528     o                       compe3p       2    170
327600030528     o                       diffe3p       j    186
327700030528     o                       tpcompe3p     j    198
327800030528     O          e            distret     0  1
327900030528     o                                           30 'Tot.'
328000060913     o                       stime3        j     46
328100030528     o                       reali3        2     60
328200060913     o                       costi3        j     74
328300030528     o                       compe3        2     88
328400030528     o                       diffe3        j    104
328500030528     o                       tpcompe3      j    116
328600030528     o
328700060913     o                       stime3p       j    128
328800030528     o                       reali3p       2    142
328900060913     o                       costi3p       j    156
329000030528     o                       compe3p       2    170
329100030528     o                       diffe3p       j    186
329200030528     o                       tpcompe3p     j    198
329300020619     Oecoprt4   e            TESTA4            2
329400020625     o                                            8 'FICN34R'
329500020625     o                       *date         y   +  3
329600030227     o                       wora              +  2 '  :  :  '
329700030319     O                                          100 'Stampa C.E. giornaliero -
329800030314     O                                              Costi variabili'
329900030319     o                                          110 'Dal '
330000020619     o                       dalladatg         +  2 '  /  /    '
330100020620     o                                         +  2 'Al '
330200020619     o                       alladatg          +  2 '  /  /    '
330300020625     o                                         +  2 'Per Azienda'
330400030227     o                                          185 'Pag.'
330500030227     o                       page              +  1
330600020625     O          e            TESTA4      0
330700030319     O                                          100 'Stampa C.E. giornaliero -
330800030314     O                                              Costi variabili'
330900030319     o                                          110 'Dal '
331000020625     o                       dalladatg         +  2 '  /  /    '
331100020625     o                                         +  2 'Al '
331200020625     o                       alladatg          +  2 '  /  /    '
331300020625     o                                         +  2 'Per Azienda'
331400020625     O          e            TESTA4      0
331500030319     O                                          100 'Stampa C.E. giornaliero -
331600030314     O                                              Costi variabili'
331700030319     o                                          110 'Dal '
331800020625     o                       dalladatg         +  2 '  /  /    '
331900020625     o                                         +  2 'Al '
332000020625     o                       alladatg          +  2 '  /  /    '
332100020625     o                                         +  2 'Per Azienda'
332200020619     O          e            testa4      2
332300020620     O                                           12 'P.Operativo'
332400020620     O                                           80 'Giornata del'
332500020620     o                       alladatg            95 '  /  /    '
332600020620     O                                          150 'Progressivo al'
332700020620     o                       alladatg           165 '  /  /    '
332800020625     O          e            testa4      0
332900020625     O                                           12 'P.Operativo'
333000020625     O                                           80 'Giornata del'
333100020625     o                       alladatg            95 '  /  /    '
333200020625     O                                          150 'Progressivo al'
333300020625     o                       alladatg           165 '  /  /    '
333400020625     O          e            testa4      0
333500020625     O                                           12 'P.Operativo'
333600020625     O                                           80 'Giornata del'
333700020625     o                       alladatg            95 '  /  /    '
333800020625     O                                          150 'Progressivo al'
333900020625     o                       alladatg           165 '  /  /    '
334000020619     O          e            testa4      1
334100060913     o                                           47 'C.Stimati'
334200030528     o                                           60 'Costi'
334300030528     o                                           74 'Costi'
334400030528     o                                           88 'Competenze'
334500030528     o                                          110 'Scostamento'
334600030528      *
334700060913     o                                          130 'C.Stimati'
334800030528     o                                          142 'Costi'
334900030528     o                                          156 'Costi'
335000030528     o                                          170 'Competenze'
335100030528     o                                          192 'Scostamento'
335200020625     O          e            testa4      0
335300060913     o                                           47 'C.Stimati'
335400030528     o                                           60 'Costi'
335500030528     o                                           74 'Costi'
335600030528     o                                           88 'Competenze'
335700030528     o                                          110 'Scostamento'
335800030528      *
335900060913     o                                          130 'C.Stimati'
336000030528     o                                          142 'Costi'
336100030528     o                                          156 'Costi'
336200030528     o                                          170 'Competenze'
336300030528     o                                          192 'Scostamento'
336400020625     O          e            testa4      0
336500060913     o                                           47 'C.Stimati'
336600030528     o                                           60 'Costi'
336700030528     o                                           74 'Costi'
336800030528     o                                           88 'Competenze'
336900030528     o                                          110 'Scostamento'
337000030528      *
337100060913     o                                          130 'C.Stimati'
337200030528     o                                          142 'Costi'
337300030528     o                                          156 'Costi'
337400030528     o                                          170 'Competenze'
337500030528     o                                          192 'Scostamento'
337600020619     O          e            testa4      1
337700060913     o                                           48 'Rettifiche'
337800030528     o                                           60 'Reali'
337900030528     o                                           74 'Totali'
338000030528     o                                          104 'Valore'
338100030528     o                                          116 'Delta %'
338200030528      *
338300060913     o                                          130 'Rettifiche'
338400030528     o                                          142 'Reali'
338500030528     o                                          156 'Totali'
338600030528     o                                          186 'Valore'
338700030528     o                                          198 'Delta %'
338800020625     O          e            testa4      0
338900060913     o                                           48 'Rettifiche'
339000030528     o                                           60 'Reali'
339100030528     o                                           74 'Totali'
339200030528     o                                          104 'Valore'
339300030528     o                                          116 'Delta %'
339400030528      *
339500060913     o                                          130 'Rettifiche'
339600030528     o                                          142 'Reali'
339700030528     o                                          156 'Totali'
339800030528     o                                          186 'Valore'
339900030528     o                                          198 'Delta %'
340000020625     O          e            testa4      0
340100060913     o                                           48 'Rettifiche'
340200030528     o                                           60 'Reali'
340300030528     o                                           74 'Totali'
340400030528     o                                          104 'Valore'
340500030528     o                                          116 'Delta %'
340600030528      *
340700060913     o                                          130 'Rettifiche'
340800030528     o                                          142 'Reali'
340900030528     o                                          156 'Totali'
341000030528     o                                          186 'Valore'
341100030528     o                                          198 'Delta %'
341200020619     O          e            detta       1
341300020620     O                       exfil                3
341400020620     o                       desfil              20
341500030527     o                                           30 'Aut.'
341600060913     o                       totstm1ds     j     46
341700030528     o                       totcos1ds     2     60
341800060913     o                       totsco1s      j     74
341900030528     o                       totcom1ds     2     88
342000030528     o                       totdif1s      j    104
342100030528     o                       totper1s      j    116
342200030528     o
342300060913     o                       totstm1ps     j    128
342400030528     o                       totcos1ps     2    142
342500060913     o                       totsco1ps     j    156
342600030528     o                       totcom1ps     2    170
342700030528     o                       totdif1ps     j    186
342800030528     o                       totper1ps     j    198
342900030115     O          e            detta       1
343000030527     o                                           30 'Coop'
343100060913     o                       totstm1dsx    j     46
343200030528     o                       totcos1dsx    2     60
343300060913     o                       totsco1sx     j     74
343400030528     o                       totcom1dsx    2     88
343500030528     o                       totdif1sx     j    104
343600030528     o                       totper1sx     j    116
343700030528     o
343800060913     o                       totstm1psx    j    128
343900030528     o                       totcos1psx    2    142
344000060913     o                       totsco1psx    j    156
344100030528     o                       totcom1psx    2    170
344200030528     o                       totdif1psx    j    186
344300030528     o                       totper1psx    j    198
344400030515     O          e            detta       1
344500030527     o                                           30 'AfDe'
344600060913     o                       totstm1dsa    j     46
344700030528     o                       totcos1dsa    2     60
344800060913     o                       totsco1sa     j     74
344900030528     o                       totcom1dsa    2     88
345000030528     o                       totdif1sa     j    104
345100030528     o                       totper1sa     j    116
345200030528     o
345300060913     o                       totstm1psa    j    128
345400030528     o                       totcos1psa    2    142
345500060913     o                       totsco1psa    j    156
345600030528     o                       totcom1psa    2    170
345700030528     o                       totdif1psa    j    186
345800030528     o                       totper1psa    j    198
345900030528     O          e            detta       1
346000030528     o                                           30 'Tot.'
346100060913     o                       stime1        j     46
346200030528     o                       reali1        2     60
346300060913     o                       costi1        j     74
346400030528     o                       compe1        2     88
346500030528     o                       diffe1        j    104
346600030528     o                       tpcompe1      j    116
346700030528     o
346800060913     o                       stime1p       j    128
346900030528     o                       reali1p       2    142
347000060913     o                       costi1p       j    156
347100030528     o                       compe1p       2    170
347200030528     o                       diffe1p       j    186
347300030528     o                       tpcompe1p     j    198
347400030528     O          e            detta       0
347500030528     o                                           30 'Tot.'
347600060913     o                       stime1        j     46
347700030528     o                       reali1        2     60
347800060913     o                       costi1        j     74
347900030528     o                       compe1        2     88
348000030528     o                       diffe1        j    104
348100030528     o                       tpcompe1      j    116
348200030528     o
348300060913     o                       stime1p       j    128
348400030528     o                       reali1p       2    142
348500060913     o                       costi1p       j    156
348600030528     o                       compe1p       2    170
348700030528     o                       diffe1p       j    186
348800030528     o                       tpcompe1p     j    198
348900030528     O          e            detta       0
349000030528     o                                           30 'Tot.'
349100060913     o                       stime1        j     46
349200030528     o                       reali1        2     60
349300060913     o                       costi1        j     74
349400030528     o                       compe1        2     88
349500030528     o                       diffe1        j    104
349600030528     o                       tpcompe1      j    116
349700030528     o
349800060913     o                       stime1p       j    128
349900030528     o                       reali1p       2    142
350000060913     o                       costi1p       j    156
350100030528     o                       compe1p       2    170
350200030528     o                       diffe1p       j    186
350300030528     o                       tpcompe1p     j    198
350400030528     O          e            area        1
350500020619     O                                           12 'Totale area'
350600020619     o                       exarea              16
350700030528     o                                           30 'Aut.'
350800060913     o                       totstm2ds     j     46
350900030528     o                       totcos2ds     2     60
351000060913     o                       totsco2s      j     74
351100030528     o                       totcom2ds     2     88
351200030528     o                       totdif2s      j    104
351300030528     o                       totper2s      j    116
351400030528     o
351500060913     o                       totstm2ps     j    128
351600030528     o                       totcos2ps     2    142
351700060913     o                       totsco2ps     j    156
351800030528     o                       totcom2ps     2    170
351900030528     o                       totdif2ps     j    186
352000030528     o                       totper2ps     j    198
352100030527     O          e            area        1
352200030528     o                       exdesa              20
352300030528     o                                           30 'Coop'
352400060913     o                       totstm2dsx    j     46
352500030528     o                       totcos2dsx    2     60
352600060913     o                       totsco2sx     j     74
352700030528     o                       totcom2dsx    2     88
352800030528     o                       totdif2sx     j    104
352900030528     o                       totper2sx     j    116
353000030528     o
353100060913     o                       totstm2psx    j    128
353200030528     o                       totcos2psx    2    142
353300060913     o                       totsco2psx    j    156
353400030528     o                       totcom2psx    2    170
353500030528     o                       totdif2psx    j    186
353600030528     o                       totper2psx    j    198
353700030528     O          e            area        1
353800030528     o                                           30 'AfDe'
353900060913     o                       totstm2dsa    j     46
354000030528     o                       totcos2dsa    2     60
354100060913     o                       totsco2sa     j     74
354200030528     o                       totcom2dsa    2     88
354300030528     o                       totdif2sa     j    104
354400030528     o                       totper2sa     j    116
354500030528     o
354600060913     o                       totstm2psa    j    128
354700030528     o                       totcos2psa    2    142
354800060913     o                       totsco2psa    j    156
354900030528     o                       totcom2psa    2    170
355000030528     o                       totdif2psa    j    186
355100030528     o                       totper2psa    j    198
355200030528     O          e            area        1
355300030528     o                                           30 'Tot.'
355400060913     o                       stime2        j     46
355500030528     o                       reali2        2     60
355600060913     o                       costi2        j     74
355700030528     o                       compe2        2     88
355800030528     o                       diffe2        j    104
355900030528     o                       tpcompe2      j    116
356000030528     o
356100060913     o                       stime2p       j    128
356200030528     o                       reali2p       2    142
356300060913     o                       costi2p       j    156
356400030528     o                       compe2p       2    170
356500030528     o                       diffe2p       j    186
356600030528     o                       tpcompe2p     j    198
356700030528     O          e            area        0
356800030528     o                                           30 'Tot.'
356900060913     o                       stime2        j     46
357000030528     o                       reali2        2     60
357100060913     o                       costi2        j     74
357200030528     o                       compe2        2     88
357300030528     o                       diffe2        j    104
357400030528     o                       tpcompe2      j    116
357500030528     o
357600060913     o                       stime2p       j    128
357700030528     o                       reali2p       2    142
357800060913     o                       costi2p       j    156
357900030528     o                       compe2p       2    170
358000030528     o                       diffe2p       j    186
358100030528     o                       tpcompe2p     j    198
358200030528     O          e            area        0  1
358300030528     o                                           30 'Tot.'
358400060913     o                       stime2        j     46
358500030528     o                       reali2        2     60
358600060913     o                       costi2        j     74
358700030528     o                       compe2        2     88
358800030528     o                       diffe2        j    104
358900030528     o                       tpcompe2      j    116
359000030528     o
359100060913     o                       stime2p       j    128
359200030528     o                       reali2p       2    142
359300060913     o                       costi2p       j    156
359400030528     o                       compe2p       2    170
359500030528     o                       diffe2p       j    186
359600030528     o                       tpcompe2p     j    198
359700030528     O          e            distret     1
359800100204     O                                           18 'Totale Dir.Comm.'
359900020620     o                       exdistr             20
360000030528     o                                           30 'Aut.'
360100060913     o                       totstm3ds     j     46
360200030528     o                       totcos3ds     2     60
360300060913     o                       totsco3s      j     74
360400030528     o                       totcom3ds     2     88
360500030528     o                       totdif3s      j    104
360600030528     o                       totper3s      j    116
360700030528     o
360800060913     o                       totstm3ps     j    128
360900030528     o                       totcos3ps     2    142
361000060913     o                       totsco3ps     j    156
361100030528     o                       totcom3ps     2    170
361200030528     o                       totdif3ps     j    186
361300030528     o                       totper3ps     j    198
361400030527     O          e            distret     1
361500030528     o                       exdesd              20
361600030528     o                                           30 'Coop'
361700060913     o                       totstm3dsx    j     46
361800030528     o                       totcos3dsx    2     60
361900060913     o                       totsco3sx     j     74
362000030528     o                       totcom3dsx    2     88
362100030528     o                       totdif3sx     j    104
362200030528     o                       totper3sx     j    116
362300030528     o
362400060913     o                       totstm3psx    j    128
362500030528     o                       totcos3psx    2    142
362600060913     o                       totsco3psx    j    156
362700030528     o                       totcom3psx    2    170
362800030528     o                       totdif3psx    j    186
362900030528     o                       totper3psx    j    198
363000030528     O          e            distret     1
363100030528     o                                           30 'AfDe'
363200060913     o                       totstm3dsa    j     46
363300030528     o                       totcos3dsa    2     60
363400060913     o                       totsco3sa     j     74
363500030528     o                       totcom3dsa    2     88
363600030528     o                       totdif3sa     j    104
363700030528     o                       totper3sa     j    116
363800030528     o
363900060913     o                       totstm3psa    j    128
364000030528     o                       totcos3psa    2    142
364100060913     o                       totsco3psa    j    156
364200030528     o                       totcom3psa    2    170
364300030528     o                       totdif3psa    j    186
364400030528     o                       totper3psa    j    198
364500030528     O          e            distret     1
364600030528     o                                           30 'Tot.'
364700060913     o                       stime3        j     46
364800030528     o                       reali3        2     60
364900060913     o                       costi3        j     74
365000030528     o                       compe3        2     88
365100030528     o                       diffe3        j    104
365200030528     o                       tpcompe3      j    116
365300030528     o
365400060913     o                       stime3p       j    128
365500030528     o                       reali3p       2    142
365600060913     o                       costi3p       j    156
365700030528     o                       compe3p       2    170
365800030528     o                       diffe3p       j    186
365900030528     o                       tpcompe3p     j    198
366000030528     O          e            distret     0
366100030528     o                                           30 'Tot.'
366200060913     o                       stime3        j     46
366300030528     o                       reali3        2     60
366400060913     o                       costi3        j     74
366500030528     o                       compe3        2     88
366600030528     o                       diffe3        j    104
366700030528     o                       tpcompe3      j    116
366800030528     o
366900060913     o                       stime3p       j    128
367000030528     o                       reali3p       2    142
367100060913     o                       costi3p       j    156
367200030528     o                       compe3p       2    170
367300030528     o                       diffe3p       j    186
367400030528     o                       tpcompe3p     j    198
367500030528     O          e            distret     0  1
367600030528     o                                           30 'Tot.'
367700060913     o                       stime3        j     46
367800030528     o                       reali3        2     60
367900060913     o                       costi3        j     74
368000030528     o                       compe3        2     88
368100030528     o                       diffe3        j    104
368200030528     o                       tpcompe3      j    116
368300030528     o
368400060913     o                       stime3p       j    128
368500030528     o                       reali3p       2    142
368600060913     o                       costi3p       j    156
368700030528     o                       compe3p       2    170
368800030528     o                       diffe3p       j    186
368900030528     o                       tpcompe3p     j    198
369000030528     O          e            azienda     1
369100020619     O                                           16 'Totale azienda'
369200030609     o                                           30 'Aut.'
369300060913     o                       totstm4ds     j     46
369400030528     o                       totcos4ds     2     60
369500060913     o                       totsco4s      j     74
369600030528     o                       totcom4ds     2     88
369700030528     o                       totdif4s      j    104
369800030528     o                       totper4s      j    116
369900030528     o
370000060913     o                       totstm4ps     j    128
370100030528     o                       totcos4ps     2    142
370200060913     o                       totsco4ps     j    156
370300030528     o                       totcom4ps     2    170
370400030528     o                       totdif4ps     j    186
370500030528     o                       totper4ps     j    198
370600030115     O          e            azienda     1
370700030609     o                                           30 'Coop'
370800060913     o                       totstm4dsx    j     46
370900030528     o                       totcos4dsx    2     60
371000060913     o                       totsco4sx     j     74
371100030528     o                       totcom4dsx    2     88
371200030528     o                       totdif4sx     j    104
371300030528     o                       totper4sx     j    116
371400030528     o
371500060913     o                       totstm4psx    j    128
371600030528     o                       totcos4psx    2    142
371700060913     o                       totsco4psx    j    156
371800030528     o                       totcom4psx    2    170
371900030528     o                       totdif4psx    j    186
372000030528     o                       totper4psx    j    198
372100030515     O          e            azienda     1
372200030609     o                                           30 'AfDe'
372300060913     o                       totstm4dsa    j     46
372400030528     o                       totcos4dsa    2     60
372500060913     o                       totsco4sa     j     74
372600030528     o                       totcom4dsa    2     88
372700030528     o                       totdif4sa     j    104
372800030528     o                       totper4sa     j    116
372900030528     o
373000060913     o                       totstm4psa    j    128
373100030528     o                       totcos4psa    2    142
373200060913     o                       totsco4psa    j    156
373300030528     o                       totcom4psa    2    170
373400030528     o                       totdif4psa    j    186
373500030528     o                       totper4psa    j    198
373600030528     O          e            azienda     1
373700030528     o                                           30 'Tot.'
373800060913     o                       stime4        j     46
373900030528     o                       reali4        2     60
374000060913     o                       costi4        j     74
374100030528     o                       compe4        2     88
374200030528     o                       diffe4        j    104
374300030528     o                       tpcompe4      j    116
374400030528     o
374500060913     o                       stime4p       j    128
374600030528     o                       reali4p       2    142
374700060913     o                       costi4p       j    156
374800030528     o                       compe4p       2    170
374900030528     o                       diffe4p       j    186
375000030528     o                       tpcompe4p     j    198
375100030528     O          e            azienda     0
375200030528     o                                           30 'Tot.'
375300060913     o                       stime4        j     46
375400030528     o                       reali4        2     60
375500060913     o                       costi4        j     74
375600030528     o                       compe4        2     88
375700030528     o                       diffe4        j    104
375800030528     o                       tpcompe4      j    116
375900030528     o
376000060913     o                       stime4p       j    128
376100030528     o                       reali4p       2    142
376200060913     o                       costi4p       j    156
376300030528     o                       compe4p       2    170
376400030528     o                       diffe4p       j    186
376500030528     o                       tpcompe4p     j    198
376600030528     O          e            azienda     0
376700030528     o                                           30 'Tot.'
376800060913     o                       stime4        j     46
376900030528     o                       reali4        2     60
377000060913     o                       costi4        j     74
377100030528     o                       compe4        2     88
377200030528     o                       diffe4        j    104
377300030528     o                       tpcompe4      j    116
377400030528     o
377500060913     o                       stime4p       j    128
377600030528     o                       reali4p       2    142
377700060913     o                       costi4p       j    156
377800030528     o                       compe4p       2    170
377900030528     o                       diffe4p       j    186
378000030528     o                       tpcompe4p     j    198
378100020612**
378200030529OVRPRTF ECOPRT1 FORMTYPE(REP_C.EC) save(*YES) OUTQ(0123456789)
378300030529OVRPRTF ECOPRT2 FORMTYPE(REP_C.EC) save(*YES) OUTQ(0123456789)
378400030529OVRPRTF ECOPRT3 FORMTYPE(REP_C.EC) save(*YES) OUTQ(0123456789)
378500050221OVRPRTF ECOPRT4 FORMTYPE(REP_C.EC) save(*YES) OUTQ(P046A135S )
378600030922CLRPFM FILE(FIFSR00F)
