000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400011031      *  Nome programma:  ficn91R
000500000000      *  Descrizione   :  Riorganizzazione tariffe padroncini
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
001700011031     Ffifgt00F  UF   E             DISK
001800011031     Ffifgt01L  IF   E           K DISK
001900011031     F                                     RENAME(fifgt000:fifgt01)
002000000000     F                                     INFDS(FGT)
002100011031     Ffifpt00F  UF   E             DISK
002200011031     Ffifpt01L  IF   E           K DISK
002300011031     F                                     RENAME(fifpt000:fifpt01)
002400000000     F                                     INFDS(FPT)
002500011031     Ffifpd00F  UF   E             DISK
002600011031     Ffifpd01L  IF   E           K DISK
002700011031     F                                     RENAME(fifpd000:fifpd01)
002800000000     F                                     INFDS(FPD)
002900000000      *
003000000000     D FGT             DS
003100000000     D  FGTNRR               397    400B 0
003200000000     D FPT             DS
003300000000     D  FPTNRR               397    400B 0
003400000000     D FPD             DS
003500000000     D  FPDNRR               397    400B 0
003600000000     D KPJBA         E DS
003700000000      *
003800000000     C     *ENTRY        PLIST
003900000000     C                   PARM                    KPJBA
004000000000      *
004100000000      *****************************************************************
004200000000      *
004300011031      *  ----------    Pulizia fifgt    ----------
004400011031     C     *LOVAL        SETLL     fifgt01L
004500011031     C                   READ      fifgt01L                               01
004600000000     C     *IN01         DOWEQ     '0'
004700000000      *
004800000000     C     FGTATB        IFEQ      'A'
004900000000      *
005000000000      *  SE LA FILIALE E' MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
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
006600000000      *  SE LA FILIALE E MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
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
008200000000      *  SE LA FILIALE E MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
008300011031     C     FPDNRR        CHAIN     fifpd00F                           3010
008400940926     C  N30
008500011031     CANN10              DELETE    fifpd000
008600000000     C                   ENDIF
008700000000      *
008800011031     C                   READ      fifpd01L                               01
008900000000     C                   ENDDO
009000000000      *
009100000000     C                   SETON                                        LR
