000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941104     H*--------------------------------------------------------------*
000300961028     H*      STAMPA ETICHETTE FATTURAZIONE AUTOMATICA
000400961028     H*                      CLIENTI CODIFICATI
000500000000     H*--------------------------------------------------------------*
000600000210     FETLAV00F  IP   E           K DISK
000700970911     FFNSPE03L  IF   E           K DISK
000800051128     FIMLAV00F  IF   E           K DISK
000900961028     FETICHET   O    F  132        PRINTER USROPN
001000051128     FQPRINT    O    F  132        PRINTER USROPN
001100000107     D KPJBA         E DS
001200000107      *
001300000107     D SPWFLS          S                   LIKE(SPEFLS) INZ('L')
001400000107     D SPWCOD          S                   LIKE(SPECOD) INZ('001')
001500030520
001600051128     D CMD             S             80    DIM(2)  CTDATA PERRCD(1)             OVRPRTF
001700051128     D SE              S             66    DIM(3)  CTDATA PERRCD(1)             FATTURE ESCLUSE MAIL
001800000207
001900000207     D WPOFAT          s              3
002000030520     D command         S             80
002100030520     D lung            S             15  5 inz(80)
002200051128     D mr              s              2  0                                      contatore righe mail
002300000000     C*---------------------------------------------------------------*
002400961029     C*
002500000000     C*---------------------------------------------------------------*
002600000209     C* RECUPERO LA FILIALE DI APPARTENENZA DEL CODICE DI FATTURAZIONE
002700000209      *
002800961028     C*
002900000209     C* AGGANCIO ANAGRAFICO LUOGHI PRIMA CON IL CODICE DI BOLLETTAZIONE E SE NON ESISTE
003000000209     C* CON QUELLO DI FATTURAZIONE
003100000107     C     KSPE          CHAIN     FNSPE03L                           91
003200000118     c   91kfat          chain     fnspe03l                           91
003300961028     C                   EXCEPT    ETIC
003400051128      *
003500051128      * a fine elaborazione delle etichette leggo il file di lavoro
003600051128      * dell'invio mail e stampo
003700051128     c     *loval        setll     IMlav00f
003800051128     c                   do        *hival
003900051128     c                   read      IMlav00f
004000051128     c                   if        %eof(imlav00f)
004100051128     c                   leave
004200051128     c                   endif
004300051128      * se contatore di riga è uguale a zero stampo la testa
004400051128     c                   if        mr = 0  or mr > 62
004500051128     c                   except    tesmai
004600051128     c                   z-add     10            mr
004700051128     c                   endif
004800051128
004900051128     c                   except    rigmai
005000051128     c                   add       2             mr
005100051128
005200051128     c                   enddo
005300051128
005400000209     C*
005500000209     C*****************************************************************
005600000209     C**     SR INZIALE
005700000209     C*****************************************************************
005800000209     C     *INZSR        BEGSR
005900000209     C*
006000000209     C     *ENTRY        PLIST
006100000209     C                   PARM                    KPJBA
006200051128
006300051128     c                   TIME                    W0140            14 0
006400051128      * UDATE IN GGMMAAAA
006500051128     C                   MOVE      W0140         WDTGIO            8 0
006600000209     C*---------------------------------------------------------------*
006700000209     C     KSPE          KLIST
006800000209     C                   KFLD                    SPWFLS
006900000209     C                   KFLD                    ETLBOL
007000000209     C                   KFLD                    SPWCOD
007100000209     C     Kfat          KLIST
007200000209     C                   KFLD                    SPWFLS
007300000209     C                   KFLD                    ETLFAT
007400000209     C                   KFLD                    SPWCOD
007500000209     C*
007600030520     c                   movea     CMD(1)        COMMAND
007700030520      *
007800030520     c                   call      'QCMDEXC'
007900030520     c                   parm                    COMMAND
008000030520     c                   parm                    LUNG
008100030520      *
008200000209     C                   OPEN      ETICHET
008300051128      *
008400051128     c                   movea     CMD(2)        COMMAND
008500051128      *
008600051128     c                   call      'QCMDEXC'
008700051128     c                   parm                    COMMAND
008800051128     c                   parm                    LUNG
008900051128      *
009000051128     C                   OPEN      QPRINT
009100000209     c*
009200000209     C                   ENDSR
009300961028     OETICHET   E            ETIC             02
009400961028     O                                              'Cliente'
009500000209     O                       ETLBOL            +  1
009600961028     O                                         +  3 'N.Fattura'
009700000209     O                       ETLNDC        Z   +  1
009800000207     O                                         +  5 '('
009900000210     O                       ETLASF
010000000207     O                                              ')'
010100961028     O          E            ETIC             04
010200970911     O                       SPERAG            +  1
010300961028     O          E            ETIC             05
010400970911     O                       SPEIND            +  1
010500961028     O          E            ETIC             06
010600970911     O                       SPECAP            +  1
010700970911     O                       SPELOC            +  0
010800970911     O                       SPEPRO            +  0
010900961028     O          E            ETIC             08
011000970911     O                                         +  1 'Att.ne '
011100961028     O                       SPEPER            +  2
011200051128      * fatture non inviate via mail ma stampata etichetta perchè supera 50 pagine
011300051128     Oqprint    E            tesmai           02
011400051128     O                       SE(1)               86
011500051128     O                                           99 'TNSF15R'
011600051128     O                       WDTGIO             115 '  /  /    '
011700051128     O          E            tesmai      2  2
011800051128     O                       SE(2)               66
011900051128     O                       SE(3)              132
012000051128     O          E            rigmai      1
012100051128     O                       imlnft        z   +  1
012200051128     O                       imldft            +  1 '  /  /  '
012300051128     O                       imlpag        z   +  1
012400051128     O                       imlcbo            +  1
012500051128     O                       imlcfa            +  1
012600051128     O                       imlind            +  1
012700051128     O                       imlinv            +  1
012800051128     O                       imleti            +  4
012900051128     O*
013000030520** CMD
013100030520OVRPRTF ETICHET SAVE(*YES) OUTQ(FATTMESE)
013200051128OVRPRTF QPRINT  SAVE(*YES) OUTQ(FATTMESE)
013300051128**  SE STAMPA ESCLUSI  via mail
013400051128         - ELENCO FATTURE CON INDIRIZZO MAIL INVIATE E NON -
013500051128Num.fatt. Dta fattur  Pagine  Cli.Bollet.   Cli.Fattur.   Indirizz
013600051128o mail                                                OK Etichetta
