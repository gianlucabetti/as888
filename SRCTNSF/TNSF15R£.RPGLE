000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941104     H*--------------------------------------------------------------*
000300961028     H*      STAMPA ETICHETTE FATTURAZIONE AUTOMATICA
000400961028     H*                      CLIENTI CODIFICATI
000500000000     H*--------------------------------------------------------------*
000600000210     FETLAV00F  IP   E           K DISK
000700970911     FFNSPE03L  IF   E           K DISK
000800961028     FETICHET   O    F  132        PRINTER USROPN
000900000107     D KPJBA         E DS
001000000107      *
001100000107     D SPWFLS          S                   LIKE(SPEFLS) INZ('L')
001200000107     D SPWCOD          S                   LIKE(SPECOD) INZ('001')
001300030520
001400030520     D CMD             S             80    DIM(1)  CTDATA PERRCD(1)             OVRPRTF
001500000207
001600000207     D WPOFAT          s              3
001700030520     D command         S             80
001800030520     D lung            S             15  5 inz(80)
001900000000     C*---------------------------------------------------------------*
002000961029     C*
002100000000     C*---------------------------------------------------------------*
002200000209     C* RECUPERO LA FILIALE DI APPARTENENZA DEL CODICE DI FATTURAZIONE
002300000209      *
002400961028     C*
002500000209     C* AGGANCIO ANAGRAFICO LUOGHI PRIMA CON IL CODICE DI BOLLETTAZIONE E SE NON ESISTE
002600000209     C* CON QUELLO DI FATTURAZIONE
002700000107     C     KSPE          CHAIN     FNSPE03L                           91
002800000118     c   91kfat          chain     fnspe03l                           91
002900961028     C                   EXCEPT    ETIC
003000000209     C*
003100000209     C*****************************************************************
003200000209     C**     SR INZIALE
003300000209     C*****************************************************************
003400000209     C     *INZSR        BEGSR
003500000209     C*
003600000209     C     *ENTRY        PLIST
003700000209     C                   PARM                    KPJBA
003800000209     C*---------------------------------------------------------------*
003900000209     C     KSPE          KLIST
004000000209     C                   KFLD                    SPWFLS
004100000209     C                   KFLD                    ETLBOL
004200000209     C                   KFLD                    SPWCOD
004300000209     C     Kfat          KLIST
004400000209     C                   KFLD                    SPWFLS
004500000209     C                   KFLD                    ETLFAT
004600000209     C                   KFLD                    SPWCOD
004700000209     C*
004800030520     c                   movea     CMD(1)        COMMAND
004900030520      *
005000030520     c                   call      'QCMDEXC'
005100030520     c                   parm                    COMMAND
005200030520     c                   parm                    LUNG
005300030520      *
005400000209     C                   OPEN      ETICHET
005500000209     c*
005600000209     C                   ENDSR
005700961028     OETICHET   E            ETIC             02
005800961028     O                                              'Cliente'
005900000209     O                       ETLBOL            +  1
006000961028     O                                         +  3 'N.Fattura'
006100000209     O                       ETLNDC        Z   +  1
006200000207     O                                         +  5 '('
006300000210     O                       ETLASF
006400000207     O                                              ')'
006500961028     O          E            ETIC             04
006600970911     O                       SPERAG            +  1
006700961028     O          E            ETIC             05
006800970911     O                       SPEIND            +  1
006900961028     O          E            ETIC             06
007000970911     O                       SPECAP            +  1
007100970911     O                       SPELOC            +  0
007200970911     O                       SPEPRO            +  0
007300961028     O          E            ETIC             08
007400970911     O                                         +  1 'Att.ne '
007500961028     O                       SPEPER            +  2
007600030520** CMD
007700030520OVRPRTF ETICHET SAVE(*YES) OUTQ(FATTMESE)
