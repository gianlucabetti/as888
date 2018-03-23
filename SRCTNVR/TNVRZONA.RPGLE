000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300160420     FFNblp01L  iF   E           K DISK    prefix(arb:3)
000400160420     FFNart01L  iF   E           K DISK
000500160420     FFNbrv07L  iF   E           K DISK
000600160420     FFNlbl01L  iF   E           K DISK
000700091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800100111
000900100111     dkpjba          e ds
001000160420     dFINE             S              1
001100021214      *--------------------------------------------------
001200100111     C     *ENTRY        PLIST
001300100111     C                   PARM                    kpjba
001400160420     c     kart          klist
001500160420     c                   kfld                    arbaas
001600160420     c                   kfld                    arblnp
001700160420     c                   kfld                    arbnrs
001800160420     c                   kfld                    arbnsp
001900160420     c     kBRV          klist
002000160420     c                   kfld                    artfls
002100160420     c                   kfld                    artlna
002200160420     c                   kfld                    artnrs
002300160420     c                   kfld                    artnsc
002400160420     c
002500160420     c                   z-add     2016          kaas              4 0
002600100111
002700160420     c     kaas          setll     fnblp01l
002800160420     c                   read      fnblp01l
002900051110
003000160420    1c                   DOW       not %EOF(FNblp01l)
003100160420    2c                   if        arbaas=2016 and arbmgs>0417
003200160420     c                             and arbnzd='   '
003300160420     C                   EVAL      FINE='0'
003400160420
003500160420     c     kart          setll     fnart01l
003600160420     c     kart          reade     fnart01l
003700160420
003800160420    3c                   DOW       not %EOF(FNarT01l) AND FINE<>'1'
003900160420     C     KBRV          SETLL     FNBRV07L
004000160420     C     KBRV          READE     FNBRV07L
004100160420
004200160420    4c                   DOW       not %EOF(FNBRV07l) AND (BRVDCS=0
004300160420     C                             OR BRVNPS>46)
004400160420     C     KBRV          READE     FNBRV07L
004500160420    4C                   ENDDO
004600160420
004700160420    4C                   IF        not %EOF(FNBRV07l) AND BRVZNC<>ARBZNC
004800160420     c                   clear                   leg               3
004900160420     c                   if        brvnrs=00
005000160420     c* verifico se legata a mamma
005100160420     c     kart          chain     fnlbl01l
005200160420     c                   if        %found(fnlbl01l)
005300160420     c                   if        arbfbr='S'
005400160420     c                   eval      leg='RIE'
005500160420     C                   ADD       1             DIV_RIE           9 0
005600160420     c                   else
005700160420     C                   ADD       1             DIV_ll            9 0
005800160420     c                   eval      leg='LEG'
005900160420     c                   endif
006000160420     c                   else
006100160420     C                   ADD       1             DIV_00            9 0
006200160420     c                   endif
006300160420     c                   endif
006400160420     c
006500160420     c                   if        brvnrs>0
006600160420     C                   ADD       1             DIV_01            9 0
006700160420     c                   endif
006800160420     c
006900160420     C                   EXCEPT    ERRORE
007000160420     C                   EVAL      FINE='1'
007100160420    4C                   ENDIF
007200160420     C
007300160420     C
007400160420     c     kart          reade     fnart01l
007500160420    3C                   ENDDO
007600160420    2C                   ENDIF
007700160420     c
007800160420     c                   read      fnblp01l
007900141106    1c                   enddo
008000021216      *
008100160420     c                   except    TOT
008200021214     C                   eval       *inlr = *on
008300100111     c
008400160420     OQSYSPRT   E            ERRORE      1
008500160420     O                                              'BOLLA'
008600100111     O                       arblnp           +   1
008700160420     O                       arbnrs        z  +   1
008800100111     O                       arbnsp        2  +   1
008900100111     O                       arblna           +   1
009000160420     O                       arbmgs           +   1 '  /  '
009100160420     O                       arbZNC           +   1
009200160420     O
009300160420     O                                        +   1 'ZONA SEGNAC.<>'
009400160420     O                       BRVZNC           +   1
009500160420     O                       leg              +   1
009600160420     OQSYSPRT   E            TOT         2
009700160420     O                                        +   2 'Totale con nrs= 00'
009800160420     o                                        +   1 'non legata'
009900160420     O                       DIV_00        2  +   1
010000160420     o                                        +   2 'legata'
010100160420     O                       DIV_ll        2  +   1
010200160420     O                                        +   2 'Totale con nrs<>00'
010300160420     O                       DIV_01        2  +   1
