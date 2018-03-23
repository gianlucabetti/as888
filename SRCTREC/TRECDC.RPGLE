000100950322     C*------------------------------------------------------------------------*
000200950322     C*     - IMPOSTA IMMAGINE PRIMA E DOPO DI UN CONTRASSEGNO DI SEDE -       *
000300950322     C*------------------------------------------------------------------------*
000400950322     C*------------------------------------------------------------------------*
000500950322     C*?APDCSQ - AZZERA TUTTI I CAMPI DELLA DS -PRIMA-
000600950322     C*------------------------------------------------------------------------*
000700950322     C     APDCSQ        BEGSR
000800950322     C*
000900950322     C                   Z-ADD     *ZEROS        DCPFLE
001000950322     C                   MOVEL     *BLANKS       DCPTBL
001100950322     C                   Z-ADD     *ZEROS        DCPLNP
001200950322     C                   Z-ADD     *ZEROS        DCPLNA
001300950322     C                   Z-ADD     *ZEROS        DCPAAS
001400950322     C                   Z-ADD     *ZEROS        DCPNRS
001500950322     C                   Z-ADD     *ZEROS        DCPNSP
001600950322     C                   Z-ADD     *ZEROS        DCPMGS
001700950322     C                   MOVEL     *BLANKS       DCPGCA
001800950322     C                   Z-ADD     *ZEROS        DCPKSC
001900950322     C                   Z-ADD     *ZEROS        DCPCDI
002000950322     C                   Z-ADD     *ZEROS        DCPCAS
002100950322     C                   MOVEL     *BLANKS       DCPVCA
002200950322     C                   Z-ADD     *ZEROS        DCPCMB
002300950322     C                   MOVEL     *BLANKS       DCPNRA
002400950322     C                   Z-ADD     *ZEROS        DCPDTE
002500950322     C                   MOVEL     *BLANKS       DCPABI
002600950322     C                   Z-ADD     *ZEROS        DCPCAB
002700950322     C                   MOVEL     *BLANKS       DCPTPA
002800950322     C                   MOVEL     *BLANKS       DCPTPI
002900950322     C                   Z-ADD     *ZEROS        DCPUDA
003000950322     C                   Z-ADD     *ZEROS        DCPDTA
003100950322     C                   Z-ADD     *ZEROS        DCPDDC
003200950322     C                   Z-ADD     *ZEROS        DCPNDT
003300950322     C                   Z-ADD     *ZEROS        DCPNRI
003400950322     C                   Z-ADD     *ZEROS        DCPDRI
003500950322     C                   Z-ADD     *ZEROS        DCPRMN
003600950322     C                   MOVEL     *BLANKS       DCPRMA
003700950322     C                   MOVEL     *BLANKS       DCPRSD
003800950322     C                   MOVEL     *BLANKS       DCPPAR
003900950322     C                   MOVEL     *BLANKS       DCPFIN
004000950322     C                   MOVEL     *BLANKS       DCPPRO
004100950322     C                   Z-ADD     *ZEROS        DCPRGF
004200950322     C                   MOVEL     *BLANKS       DCPFPC
004300950322     C                   Z-ADD     *ZEROS        DCPBNA
004400950322     C                   Z-ADD     *ZEROS        DCPNDP
004500950322     C                   Z-ADD     *ZEROS        DCPDDP
004600950322     C                   Z-ADD     *ZEROS        DCPNRG
004700950322     C                   Z-ADD     *ZEROS        DCPDRG
004800950322     C                   Z-ADD     *ZEROS        DCPCMP
004900950322     C                   MOVEL     *BLANKS       DCPFTR
005000950322     C                   Z-ADD     *ZEROS        DCPDTR
005100950322     C                   Z-ADD     *ZEROS        DCPSTA
005200950322     C                   Z-ADD     *ZEROS        DCPFGC
005300950322     C                   Z-ADD     *ZEROS        DCPNGC
005400950322     C                   Z-ADD     *ZEROS        DCPDGC
005500950322     C                   MOVEL     *BLANKS       DCPMAS
005600950322     C                   MOVEL     *BLANKS       DCPFUS
005700950322     C                   MOVEL     *BLANKS       DCPXX1
005800950322     C                   MOVEL     *BLANKS       DCPXX2
005900950322     C*
006000950322     C                   ENDSR
006100950322     C*------------------------------------------------------------------------*
006200950322     C*?ADDCSQ - AZZERA TUTTI I CAMPI DELLA DS -DOPO-
006300950322     C*------------------------------------------------------------------------*
006400950322     C     ADDCSQ        BEGSR
006500950322     C*
006600950322     C                   Z-ADD     *ZEROS        DCDFLE
006700950322     C                   MOVEL     *BLANKS       DCDTBL
006800950322     C                   Z-ADD     *ZEROS        DCDLNP
006900950322     C                   Z-ADD     *ZEROS        DCDLNA
007000950322     C                   Z-ADD     *ZEROS        DCDAAS
007100950322     C                   Z-ADD     *ZEROS        DCDNRS
007200950322     C                   Z-ADD     *ZEROS        DCDNSP
007300950322     C                   Z-ADD     *ZEROS        DCDMGS
007400950322     C                   MOVEL     *BLANKS       DCDGCA
007500950322     C                   Z-ADD     *ZEROS        DCDKSC
007600950322     C                   Z-ADD     *ZEROS        DCDCDI
007700950322     C                   Z-ADD     *ZEROS        DCDCAS
007800950322     C                   MOVEL     *BLANKS       DCDVCA
007900950322     C                   Z-ADD     *ZEROS        DCDCMB
008000950322     C                   MOVEL     *BLANKS       DCDNRA
008100950322     C                   Z-ADD     *ZEROS        DCDDTE
008200950322     C                   MOVEL     *BLANKS       DCDABI
008300950322     C                   Z-ADD     *ZEROS        DCDCAB
008400950322     C                   MOVEL     *BLANKS       DCDTPA
008500950322     C                   MOVEL     *BLANKS       DCDTPI
008600950322     C                   Z-ADD     *ZEROS        DCDUDA
008700950322     C                   Z-ADD     *ZEROS        DCDDTA
008800950322     C                   Z-ADD     *ZEROS        DCDDDC
008900950322     C                   Z-ADD     *ZEROS        DCDNDT
009000950322     C                   Z-ADD     *ZEROS        DCDNRI
009100950322     C                   Z-ADD     *ZEROS        DCDDRI
009200950322     C                   Z-ADD     *ZEROS        DCDRMN
009300950322     C                   MOVEL     *BLANKS       DCDRMA
009400950322     C                   MOVEL     *BLANKS       DCDRSD
009500950322     C                   MOVEL     *BLANKS       DCDPAR
009600950322     C                   MOVEL     *BLANKS       DCDFIN
009700950322     C                   MOVEL     *BLANKS       DCDPRO
009800950322     C                   Z-ADD     *ZEROS        DCDRGF
009900950322     C                   MOVEL     *BLANKS       DCDFPC
010000950322     C                   Z-ADD     *ZEROS        DCDBNA
010100950322     C                   Z-ADD     *ZEROS        DCDNDP
010200950322     C                   Z-ADD     *ZEROS        DCDDDP
010300950322     C                   Z-ADD     *ZEROS        DCDNRG
010400950322     C                   Z-ADD     *ZEROS        DCDDRG
010500950322     C                   Z-ADD     *ZEROS        DCDCMP
010600950322     C                   MOVEL     *BLANKS       DCDFTR
010700950322     C                   Z-ADD     *ZEROS        DCDDTR
010800950322     C                   Z-ADD     *ZEROS        DCDSTA
010900950322     C                   Z-ADD     *ZEROS        DCDFGC
011000950322     C                   Z-ADD     *ZEROS        DCDNGC
011100950322     C                   Z-ADD     *ZEROS        DCDDGC
011200950322     C                   MOVEL     *BLANKS       DCDMAS
011300950322     C                   MOVEL     *BLANKS       DCDFUS
011400950322     C                   MOVEL     *BLANKS       DCDXX1
011500950322     C                   MOVEL     *BLANKS       DCDXX2
011600950322     C*
011700950322     C                   ENDSR
011800950322     C*------------------------------------------------------------------------*
011900950322     C*?CPDCSQ - IMPOSTA TUTTI I CAMPI DELLA DS -PRIMA-
012000950322     C*------------------------------------------------------------------------*
012100950322     C     CPDCSQ        BEGSR
012200950322     C*
012300950322     C                   Z-ADD     CSBFLE        DCPFLE
012400950322     C                   MOVEL     CSBTBL        DCPTBL
012500950322     C                   Z-ADD     CSBLNP        DCPLNP
012600950322     C                   Z-ADD     CSBLNA        DCPLNA
012700950322     C                   Z-ADD     CSBAAS        DCPAAS
012800950322     C                   Z-ADD     CSBNRS        DCPNRS
012900950322     C                   Z-ADD     CSBNSP        DCPNSP
013000950322     C                   Z-ADD     CSBMGS        DCPMGS
013100950322     C                   MOVEL     CSBGCA        DCPGCA
013200950322     C                   Z-ADD     CSBKSC        DCPKSC
013300950322     C                   Z-ADD     CSBCDI        DCPCDI
013400950322     C                   Z-ADD     CSBCAS        DCPCAS
013500950322     C                   MOVEL     CSBVCA        DCPVCA
013600950322     C                   Z-ADD     CSBCMB        DCPCMB
013700950322     C                   MOVEL     CSBNRA        DCPNRA
013800950322     C                   Z-ADD     CSBDTE        DCPDTE
013900950322     C                   MOVEL     CSBABI        DCPABI
014000950322     C                   Z-ADD     CSBCAB        DCPCAB
014100950322     C                   MOVEL     CSBTPA        DCPTPA
014200950322     C                   MOVEL     CSBTPI        DCPTPI
014300950322     C                   Z-ADD     CSBUDA        DCPUDA
014400950322     C                   Z-ADD     CSBDTA        DCPDTA
014500950322     C                   Z-ADD     CSBDDC        DCPDDC
014600950322     C                   Z-ADD     CSBNDT        DCPNDT
014700950322     C                   Z-ADD     CSBNRI        DCPNRI
014800950322     C                   Z-ADD     CSBDRI        DCPDRI
014900950322     C                   Z-ADD     CSBRMN        DCPRMN
015000950322     C                   MOVEL     CSBRMA        DCPRMA
015100950322     C                   MOVEL     CSBRSD        DCPRSD
015200950322     C                   MOVEL     CSBPAR        DCPPAR
015300950322     C                   MOVEL     CSBFIN        DCPFIN
015400950322     C                   MOVEL     CSBPRO        DCPPRO
015500950322     C                   Z-ADD     CSBRGF        DCPRGF
015600950322     C                   MOVEL     CSBFPC        DCPFPC
015700950322     C                   Z-ADD     CSBBNA        DCPBNA
015800950322     C                   Z-ADD     CSBNDP        DCPNDP
015900950322     C                   Z-ADD     CSBDDP        DCPDDP
016000950322     C                   Z-ADD     CSBNRG        DCPNRG
016100950322     C                   Z-ADD     CSBDRG        DCPDRG
016200950322     C                   Z-ADD     CSBCMP        DCPCMP
016300950322     C                   MOVEL     CSBFTR        DCPFTR
016400950322     C                   Z-ADD     CSBDTR        DCPDTR
016500950322     C                   Z-ADD     CSBSTA        DCPSTA
016600950322     C                   Z-ADD     CSBFGC        DCPFGC
016700950322     C                   Z-ADD     CSBNGC        DCPNGC
016800950322     C                   Z-ADD     CSBDGC        DCPDGC
016900950322     C                   MOVEL     CSBMAS        DCPMAS
017000950322     C                   MOVEL     CSBFUS        DCPFUS
017100950323     C                   MOVEL     *BLANKS       DCPXX1
017200950323     C                   MOVEL     *BLANKS       DCPXX2
017300950322     C*
017400950322     C                   ENDSR
017500950322     C*------------------------------------------------------------------------*
017600950322     C*?CDDCSQ - IMPOSTA TUTTI I CAMPI DELLA DS -DOPO-
017700950322     C*------------------------------------------------------------------------*
017800950322     C     CDDCSQ        BEGSR
017900950322     C*
018000950322     C                   Z-ADD     CSBFLE        DCDFLE
018100950322     C                   MOVEL     CSBTBL        DCDTBL
018200950322     C                   Z-ADD     CSBLNP        DCDLNP
018300950322     C                   Z-ADD     CSBLNA        DCDLNA
018400950322     C                   Z-ADD     CSBAAS        DCDAAS
018500950322     C                   Z-ADD     CSBNRS        DCDNRS
018600950322     C                   Z-ADD     CSBNSP        DCDNSP
018700950322     C                   Z-ADD     CSBMGS        DCDMGS
018800950322     C                   MOVEL     CSBGCA        DCDGCA
018900950322     C                   Z-ADD     CSBKSC        DCDKSC
019000950322     C                   Z-ADD     CSBCDI        DCDCDI
019100950322     C                   Z-ADD     CSBCAS        DCDCAS
019200950322     C                   MOVEL     CSBVCA        DCDVCA
019300950322     C                   Z-ADD     CSBCMB        DCDCMB
019400950322     C                   MOVEL     CSBNRA        DCDNRA
019500950322     C                   Z-ADD     CSBDTE        DCDDTE
019600950322     C                   MOVEL     CSBABI        DCDABI
019700950322     C                   Z-ADD     CSBCAB        DCDCAB
019800950322     C                   MOVEL     CSBTPA        DCDTPA
019900950322     C                   MOVEL     CSBTPI        DCDTPI
020000950322     C                   Z-ADD     CSBUDA        DCDUDA
020100950322     C                   Z-ADD     CSBDTA        DCDDTA
020200950322     C                   Z-ADD     CSBDDC        DCDDDC
020300950322     C                   Z-ADD     CSBNDT        DCDNDT
020400950322     C                   Z-ADD     CSBNRI        DCDNRI
020500950322     C                   Z-ADD     CSBDRI        DCDDRI
020600950322     C                   Z-ADD     CSBRMN        DCDRMN
020700950322     C                   MOVEL     CSBRMA        DCDRMA
020800950322     C                   MOVEL     CSBRSD        DCDRSD
020900950322     C                   MOVEL     CSBPAR        DCDPAR
021000950322     C                   MOVEL     CSBFIN        DCDFIN
021100950322     C                   MOVEL     CSBPRO        DCDPRO
021200950322     C                   Z-ADD     CSBRGF        DCDRGF
021300950322     C                   MOVEL     CSBFPC        DCDFPC
021400950322     C                   Z-ADD     CSBBNA        DCDBNA
021500950322     C                   Z-ADD     CSBNDP        DCDNDP
021600950322     C                   Z-ADD     CSBDDP        DCDDDP
021700950322     C                   Z-ADD     CSBNRG        DCDNRG
021800950322     C                   Z-ADD     CSBDRG        DCDDRG
021900950322     C                   Z-ADD     CSBCMP        DCDCMP
022000950322     C                   MOVEL     CSBFTR        DCDFTR
022100950322     C                   Z-ADD     CSBDTR        DCDDTR
022200950322     C                   Z-ADD     CSBSTA        DCDSTA
022300950322     C                   Z-ADD     CSBFGC        DCDFGC
022400950322     C                   Z-ADD     CSBNGC        DCDNGC
022500950322     C                   Z-ADD     CSBDGC        DCDDGC
022600950322     C                   MOVEL     CSBMAS        DCDMAS
022700950322     C                   MOVEL     CSBFUS        DCDFUS
022800950323     C                   MOVEL     *BLANKS       DCDXX1
022900950323     C                   MOVEL     *BLANKS       DCDXX2
023000950322     C*
023100950322     C                   ENDSR
023200950322     C*------------------------------------------------------------------------*
