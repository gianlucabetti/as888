000100000920     H DECEDIT('0,') DATEDIT(*DMY.)
000200000920     H*------------------------------------------------------------------------*
000300000920     H* CREAZIONE TARIFFA PARTICOLARE  + TARIFFE GIACENZE DA CARTELLO          *
000400000920     H*                                                                        *
000500000920     H* VENGONO PASSATI A PGM I SEGUENTI PARAMETRI :                           *
000600000920     H*                                                                        *
000700000920     H* -- Codice cliente (di solito è la tariffa di cartello)                 *
000800000920     H* -- Codice Tariffa                                                      *
000900000920     H* -- Progressivo tariffa                                                 *
001000000920     H* -- Sigla della tariffa particolare da inglobare                        *
001100000920     H* -- Flag tariffa Italia / Estero (che può essere anche blanks nel caso  *
001200000920     H*    si inglobino entrambe)                                              *
001300000920     H* -- Flag Tariffa o Offerta (' '=Tariffa , 'O'=Offerta)
001400000920     H* -- Azienda
001500000920     H*                                                                        *
001600000920     H* MI VIENE PASSATA A PROGRAMMA L'AZIENDA
001700000920     H*------------------------------------------------------------------------*
001800000920     F*
001900000920     D ftc             S              2    DIM(12) CTDATA PERRCD(12)            tarif.partic.da ingl
002000000920      *
002100000920     D parksc          s              7    inz('8888830')
002200000920     D parctr          s              3    inz('000')
002300000920     D parprg          s              3    inz('005')
002400000920     D parftc          s              2
002500000920     D parfie          s              1    inz(' ')
002600000920     D paroff          s              1
002700000920     D pardit          s              3
002800000920
002900000920     D x               s              2  0
003000000920      *
003100000920     c     *entry        plist
003200000920     c                   parm                    pardit
003300000920      *
003400000920     c* lancio delle tariffe partcolari
003500000920     c*
003600000920     c                   z-add     1             x
003700000920     c*
003800000920     c                   dow       x <= 12
003900000920      *
004000000920     c                   eval      parftc = ftc(x)
004100000920     c                   clear                   paroff
004200000920     c                   call      'TNVR48C'
004300000920     c                   parm                    parksc
004400000920     c                   parm                    parctr
004500000920     c                   parm                    parprg
004600000920     c                   parm                    parftc
004700000920     c                   parm                    parfie
004800000920     c                   parm                    paroff
004900000920     c                   parm                    pardit
005000000920      *
005100000920     c                   add       1             x
005200000920      *
005300000920     c                   enddo
005400000920      * conversione TARIFFE giacenze
005500000920     c                   call      'TNVR17R'
005600000920      * conversione OFFERTE giacenze
005700000920     c                   call      'TNVR18R'
005800000920
005900000920     C                   SETON                                        LR
006000000920**  FTC
006100000920I K A B F P S E H U V W
