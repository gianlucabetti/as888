000001090211     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000      *****************************************************************
000300000000      *
000400011031      *  Nome programma:  ficn91R
000500090624      *  Descrizione   :  Riorganizzazione tariffe autisti
000600000000      *                   Programma di esecuzione
000700000000      *
000800000000      *****************************************************************
000900000000      * RIEPILOGO INDICATORI
001000000000      *
001100000000      * 01/04 - DI COMODO
001200000000      * 30    - DI COMODO
001300000000      * 10    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD : SE
001400000000      *         IL RECORD E' ALLOCATO (CIOE' *IN10 ACCESO) SALTO LA
001500000000      *         TARIFFA (CHE VERRA' ANNULLATA LA PROSSIMA VOLTA)
001600000000      *****************************************************************
001601090211     Ffitgt01L  uF   E           K DISK    commit
001700090211     Ffifgt00F  UF   E             DISK    commit
001800011031     Ffifgt01L  IF   E           K DISK
001900011031     F                                     RENAME(fifgt000:fifgt01)
002000000000     F                                     INFDS(FGT)
002100090211     Ffifpt00F  UF   E             DISK    commit
002200011031     Ffifpt01L  IF   E           K DISK
002300011031     F                                     RENAME(fifpt000:fifpt01)
002400000000     F                                     INFDS(FPT)
002500090211     Ffifpd00F  UF   E             DISK    commit
002600011031     Ffifpd01L  IF   E           K DISK
002700011031     F                                     RENAME(fifpd000:fifpd01)
002800000000     F                                     INFDS(FPD)
002801090624     Ffiadt00F  UF   E             DISK    commit
002802090624     Ffiadt01L  IF   E           K DISK
002803090624     F                                     RENAME(fiadt000:fiadt01)
002804090624     F                                     INFDS(adt)
002805090624     Ffiadd00F  UF   E             DISK    commit
002806090624     Ffiadd01L  IF   E           K DISK
002807090624     F                                     RENAME(fiadd000:fiadd01)
002808090624     F                                     INFDS(add)
002900000000      *
003000000000     D FGT             DS
003100000000     D  FGTNRR               397    400B 0
003200000000     D FPT             DS
003300000000     D  FPTNRR               397    400B 0
003400000000     D FPD             DS
003500000000     D  FPDNRR               397    400B 0
003501090624     D adt             DS
003502090624     D  adtNRR               397    400B 0
003503090624     D add             DS
003504090624     D  addNRR               397    400B 0
003600000000     D KPJBA         E DS
003700000000      *
003800000000     C     *ENTRY        PLIST
003900000000     C                   PARM                    KPJBA
004000000000      *
004100000000      *****************************************************************
004102090114      *  ----------    Pulizia fitgt    ----------
004103090114     C     *LOVAL        SETLL     fitgt01L
004104090114     C                   READ      fitgt01L                               01
004105090114     C     *IN01         DOWEQ     '0'
004106090114     C     TGTATB        IFEQ      'A'
004111090114     C                   DELETE    fitgt000
004112090114     C                   ENDIF
004114090114     C                   READ      fitgt01L                               01
004115090114     C                   ENDDO
004200000000      *
004300011031      *  ----------    Pulizia fifgt    ----------
004400011031     C     *LOVAL        SETLL     fifgt01L
004500011031     C                   READ      fifgt01L                               01
004600000000     C     *IN01         DOWEQ     '0'
004700000000      *
004800000000     C     FGTATB        IFEQ      'A'
004900000000      *
005100011031     C     FGTNRR        CHAIN     fifgt00F                           3010
005200940926     C  N30
005300011031     CANN10              DELETE    fifgt000
005400000000     C                   ENDIF
005500000000      *
005600011031     C                   READ      fifgt01L                               01
005700000000     C                   ENDDO
005800000000      *
005900011031      *  ----------    Pulizia fifpt    ----------
006000011031     C     *LOVAL        SETLL     fifpt01L
006100011031     C                   READ      fifpt01L                               01
006200000000     C     *IN01         DOWEQ     '0'
006300000000      *
006400000000     C     FPTATB        IFEQ      'A'
006500000000      *
006700011031     C     FPTNRR        CHAIN     fifpt00F                           3010
006800940926     C  N30
006900011031     CANN10              DELETE    fifpt000
007000000000     C                   ENDIF
007100000000      *
007200011031     C                   READ      fifpt01L                               01
007300000000     C                   ENDDO
007400000000      *
007500011031      *  ----------    Pulizia fifpd    ----------
007600011031     C     *LOVAL        SETLL     fifpd01L
007700011031     C                   READ      fifpd01L                               01
007800000000     C     *IN01         DOWEQ     '0'
007900000000      *
008000000000     C     FPDATB        IFEQ      'A'
008100000000      *
008300011031     C     FPDNRR        CHAIN     fifpd00F                           3010
008400940926     C  N30
008500011031     CANN10              DELETE    fifpd000
008600000000     C                   ENDIF
008700000000      *
008800011031     C                   READ      fifpd01L                               01
008900000000     C                   ENDDO
008901090706      *
008902090706     c                   commit
008910090624      *
008911090624      *  ----------    Pulizia fiadt    ----------
008912090624     C     *LOVAL        SETLL     fiadt01L
008913090624     C                   READ      fiadt01L                               01
008914090624     C     *IN01         DOWEQ     '0'
008915090624      *
008916090624     C     adtATB        IFEQ      'A'
008917090624      *
008919090624     C     adtNRR        CHAIN     fiadt00F                           3010
008920090624     C  N30
008921090624     CANN10              DELETE    fiadt000
008922090624     C                   ENDIF
008923090624      *
008924090624     C                   READ      fiadt01L                               01
008925090624     C                   ENDDO
008926090624      *
008927090624      *  ----------    Pulizia fiadd    ----------
008928090624     C     *LOVAL        SETLL     fiadd01L
008929090624     C                   READ      fiadd01L                               01
008930090624     C     *IN01         DOWEQ     '0'
008931090624      *
008932090624     C     addATB        IFEQ      'A'
008933090624      *
008935090624     C     addNRR        CHAIN     fiadd00F                           3010
008936090624     C  N30
008937090624     CANN10              DELETE    fiadd000
008938090624     C                   ENDIF
008939090624      *
008940090624     C                   READ      fiadd01L                               01
008941090624     C                   ENDDO
008942090624      *
008958090211     c                   commit
009000000000      *
009100000000     C                   SETON                                        LR
