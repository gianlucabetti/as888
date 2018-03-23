000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941104     H*--------------------------------------------------------------*
000300961028     H*      STAMPA ETICHETTE FATTURAZIONE AUTOMATICA
000400961028     H*                      CLIENTI CODIFICATI
000500000000     H*--------------------------------------------------------------*
000600970911     FFNSPE03L  IF   E           K DISK
000700961028     FETICHET   O    F  132        PRINTER USROPN
000800000107     D TNSF14DS      E DS
000900000107     D KPJBA         E DS
001000000107      *
001100000107     D SPWFLS          S                   LIKE(SPEFLS) INZ('L')
001200000107     D SPWCOD          S                   LIKE(SPECOD) INZ('001')
001300000000     C*---------------------------------------------------------------*
001400000000     C     *ENTRY        PLIST
001500000000     C                   PARM                    KPJBA
001600000107     C                   PARM                    TNSF14DS
001700961029     C*---------------------------------------------------------------*
001800961029     C     KSPE          KLIST
001900970911     C                   KFLD                    SPWFLS
002000000107     C                   KFLD                    D14KSC
002100000107     C                   KFLD                    SPWCOD
002200000118     C     Kfat          KLIST
002300000118     C                   KFLD                    SPWFLS
002400000118     C                   KFLD                    D14fat
002500000118     C                   KFLD                    SPWCOD
002600961029     C*
002700000000     C*---------------------------------------------------------------*
002800000107     C* Se Tipo lancio = 'P' apro solo il file etichette
002900000107     c                   if        d14tla = 'P'
003000961029     C                   MOVE      KNSIF         COMOD             4
003100961029     C                   MOVEL     COMOD         KCDSI             3
003200961029     C*
003300961028     C                   MOVEL     'ETICHET'     KMDST
003400961028     C                   MOVE      KCDSI         KMDST
003500961028     C                   OPEN      ETICHET
003600000107     c                   endif
003700961028     C*
003800000107     C* Se Tipo lancio = ' ' elaboro
003900000107     c                   if        d14tla = ' '
004000000107     C     KSPE          CHAIN     FNSPE03L                           91
004100000118     c   91kfat          chain     fnspe03l                           91
004200961028     C                   EXCEPT    ETIC
004300000107     c                   endif
004400961028     C**
004500000107     C** Se ti po lancio = 'C' chiudo tutto
004600000107     c                   if        d14tla = 'C'
004700000000     C                   SETON                                        LR
004800000107     c                   else
004900000107     c                   RETURN
005000000107     c                   endif
005100961028     OETICHET   E            ETIC             02
005200961028     O                                              'Cliente'
005300000107     O                       D14KSC            +  1
005400961028     O                                         +  3 'N.Fattura'
005500000112     O                       D14NFT        Z   +  1
005600961028     O          E            ETIC             04
005700970911     O                       SPERAG            +  1
005800961028     O          E            ETIC             05
005900970911     O                       SPEIND            +  1
006000961028     O          E            ETIC             06
006100970911     O                       SPECAP            +  1
006200970911     O                       SPELOC            +  0
006300970911     O                       SPEPRO            +  0
006400961028     O          E            ETIC             08
006500970911     O                                         +  1 'Att.ne '
006600961028     O                       SPEPER            +  2
