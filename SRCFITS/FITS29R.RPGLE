000100040804     H DECEDIT('0,') DATEDIT(*ymd.)
000200040921      * FNTS29R *----------------------------------------------------*
000300040921      *  Bolle da cliente: FIVAB FIVAD FIVAT
000400030211      *--------------------------------------------------------------*
000500040921     FFIVAB02L  iF   E           K DISK
000600040921     FFIVAD01L  iF   E           K DISK
000700040921     fFIVAT01L  if   e           k disk
000800040921     fFIVAX01L  if   e           k disk
000900030211      *
001000040921     FFIVABnew  iF a E           K DISK
001100040921     F                                     RENAME(FIVAB000:VABNEW)
001200040921     FFIVABnw4  iF   E           K DISK
001300040921     F                                     RENAME(FIVAB000:VAB4NEW)
001400040921     FFIVADnew  o  a E           K DISK
001500040921     F                                     RENAME(FIVAD000:VADNEW)
001600040921     FFIVATnew  o  a E           K DISK
001700040921     F                                     RENAME(FIVAT000:VATNEW)
001800040921     FFIVAXnew  o  a E           K DISK
001900040921     F                                     RENAME(FIVAX000:VAXNEW)
002000040921     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002100040804     D* Schiera p.o. da trasferire
002200040804     D  SKPO           s              3    dim(500)
002300040921     d KLIN            s                   like(vabLNP)
002400030211      *
002500900511     D KPJBA         E DS
002600900523     C*****************************************************************
002700000000     C     *ENTRY        PLIST
002800000000     C                   PARM                    KPJBA
002900040804     C                   parm                    PARAM          1500
003000040804     c
003100040804     c     ' ':'0'       xlate     param         param
003200040804     c
003300040804     c                   movea     param         skpo
003400900511     C*---------------------------------------------------------------*
003500040921     C     kVAB2         KLIST
003600040921     C                   KFLD                    vabfgs
003700040921     C                   KFLD                    vabccm
003800040921     C                   KFLD                    vabrmn
003900040921     C     kVAb4         KLIST
004000040921     C                   KFLD                    vabaas
004100040921     C                   KFLD                    vablnp
004200040921     C                   KFLD                    vabnrs
004300040921     C                   KFLD                    vabnsp
004400040921     C     kVAd          KLIST
004500040921     C                   KFLD                    vabfgs
004600040921     C                   KFLD                    vabccm
004700040921     C                   KFLD                    vabaas
004800040921     C                   KFLD                    vablnp
004900040921     C                   KFLD                    vabnrs
005000040921     C                   KFLD                    vabnsp
005100950621     C*---------------------------------------------------------------*
005200040804     c                   z-add     1             x                 4 0
005300040804    1c                   dow       skpo(x)>*zeros
005400040804     c                   movel     skpo(X)       klin
005500040921     C     klin          SETLL     fivab02l
005600040921     C     klin          READe     fivab02l                               30
005700040804     C*
005800040804    2C     *IN30         DOWEQ     *OFF
005900040804     C*
006000040804     C** ELABORAZIONE RECORDS RICEVUTI DELLE BOLLE
006100040921     C                   EXSR      elabora
006200040804     C*
006300040921     C     KLIN          READE     Fivab02l                               30
006400040804    2C                   ENDDO
006500040804     C                   ADD       1             X
006600040804     C                   ENDDO
006700900523     C*
006800040921     c                   EXCEPT    RINUM1
006900900523     C                   SETON                                        LR
007000040804     C**-------------------------------------------------------------**
007100040921     C**  elabora i dati                                             **
007200040804     C**-------------------------------------------------------------**
007300040921     C     elabora       BEGSR
007400040804     C*
007500040921     c                   if        vabnsp=0
007600040921     C     Kvab2         SETLL     fivabnew                               32
007700040921     c                   else
007800040921     c     kvab4         setll     fivabnw4                               32
007900040921     c                   endif
008000040804     C* se non trovata
008100040921    0C     *IN32         IFEQ      *OFF
008200040921     c                   write     vabnew
008300040921     c                   add       1             totvab            7 0
008400040804     C*
008500040921     C****  fivad00F  ****
008600040921     C     Kvad          SETLL     Fivad01l
008700040921     C     Kvad          READE     Fivad01l                               31
008800040804     C*
008900040804    2C     *IN31         DOWEQ     *OFF
009000040921     C                   WRITE     vadnew                               99
009100040921     c                   add       1             totvad            7 0
009200040921     C     Kvad          READE     fivad01l                               31
009300040804    2C                   ENDDO
009400040921     C****  fivat00F  ****
009500040921     C     Kvad          SETLL     Fivat01l
009600040921     C     Kvad          READE     Fivat01l                               31
009700040921     C*
009800040921    2C     *IN31         DOWEQ     *OFF
009900040921     C                   WRITE     vatnew                               99
010000040921     c                   add       1             totvat            7 0
010100040921     C     Kvad          READE     fivat01l                               31
010200040921    2C                   ENDDO
010300040804      *
010400040921     C****  fivaX00F  ****
010500040921     C     Kvad          SETLL     FivaX01l
010600040921     C     Kvad          READE     FivaX01l                               31
010700040921     C*
010800040921    2C     *IN31         DOWEQ     *OFF
010900040921     C                   WRITE     vaXnew                               99
011000040921     c                   add       1             totvaX            7 0
011100040921     C     Kvad          READE     fivaX01l                               31
011200040921    2C                   ENDDO
011300040921     c                   endif
011400040921      *
011500040804     C                   ENDSR
011600040921     C**-------------------------------------------------------------**
011700040921     oQSYSPRT   E            RINUM1         2 02
011800040921     o                                         +  5 'BOLLE CLIENTE'
011900040921     o                                         +  5 'Fits29R'
012000040921     o                       UDATE             +  2 '  /  /  '
012100040921     o          E            RINUM1      1
012200040921     o                                         +  3 'FIVAB00F:'
012300040921     o                       TOTVAB        2   +  2
012400040921     o                                         +  3 'FIVAD00F:'
012500040921     o                       TOTVAd        2   +  2
012600040921     o                                         +  3 'FIVAT00F:'
012700040921     o                       TOTVAt        2   +  2
012800040921     o                                         +  3 'FIVAX00F:'
012900040921     o                       TOTVAx        2   +  2
