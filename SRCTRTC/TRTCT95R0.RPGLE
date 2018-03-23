000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600140220     Ftrtct95w  O    E             DISK
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100140225
001200140225     d CED_Bart        C                   CONST('CEDAlert@Brt.it;')
001300140225      *----------------------------------------------------------------------*
001400991027
001500060804     C                   exsr      traduci
001600140225
001700060804     C                   seton                                        LR
001800991027
001900140220      *----------------------------------------------------------------------*
002000991027     C     traduci       BEGSR
002100991027     C*
002200060804     C                   read      TIVGDTMP
002300991027     C                   dow       not %eof
002400121023     C*
002500121023     C* Se nei parametri non è richiesta "elaborazione fittizia" procedo
002600121023     C                   if        %subst(prmppt:1:1) <> 'K'
002700050121     C*
002800050121     C* Inizializzazione campi e variabili d out
002900060804     C                   clear                   tivaw000
003000060804     C*
003100060804     C* Valorizzo il buffer del file d output
003200080130     C                   eval      vawDTA = vgdDTA
003300140225     C*
003400140225     C*  campi file di WRK HAGER (come un TIVGD)
003500140225     C                   eval        FLATREC   = vgdDTA
003600140225     C                   eval        w95TIP    = vgdTIP
003700140225     C                   eval        w95KSU    = vgdKSU
003800140225     C                   eval        w95TSC    = vgdTSC
003900140225     C                   eval        w95DAT    = vgdDAT
004000140225     C                   eval        w95PGM    = vgdPGM
004100140225     C                   eval        w95PRG    = vgdPRG
004200140225     C                   eval        w95CNT    = vgdCNT
004300140225     C                   eval        w95STO    = vgdSTO
004400140225     C                   eval        w95FLO    = vgdFLO
004500140225     C                   eval        w95ATTRIB = vgdATTRIB
004600140225     C                   WRITE     TRTCT95WR
004700140225     C*
004800140226      **  x NON INVARE al CLIENTE anche il IFTSTA
004900140226      **    salta la scrittura .  NON la voglio cancellare perchè NON si sa mai
005000140226      **    x il futuro .... se il cliente cambiasse idea...
005100140226     c                   goto      NO_write
005200050121     C                   WRITE     TIVAW000
005300140226     c     NO_write      tag
005400121023     C*
005500121023     C                   endif
005600121023     C*
005700050909     C                   DELETE    TIVGD000
005800050909     C                   READ      TIVGDTMP
005900991027     C                   ENDdo
006000991027     C*
006100991027     C                   eval      wrkesito = '0'
006200991027     C*
006300910830     C                   ENDSR
006400060804
006500991027
006600991027      /TITLE Operazioni iniziali.
006700991027     C     *inzsr        BEGSR
006800991027     C*
006900991027     C     *ENTRY        PLIST
007000991027     C                   parm                    prmppt
007100991027     C     wrkesito      parm      wrkesito      prmesito
007200991027     C*
007300991027     C                   ENDSR
007400140225      * ?------------------------------------------------------------------ */
007500140225      *?      X non bloccare in nessun caso il traduttore CLIENTI
007600140225      * ?------------------------------------------------------------------ */
007700140225     C     *pssr         BEGSR
007800140225     C
007900140225     C                   eval      prmesito = '2'
008000140225     c                   exsr      invio_mail
008100140225     C                   ENDSR     '*CANCL'
008200140225     C
008300140225     c*==================================================================*
008400140225      * Manda un Msg x E-mail
008500140225     c*==================================================================*
008600140225     c     Invio_Mail    begsr
008700140225      *
008800140225     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
008900140225     C* Inizializzo variabili
009000140225     C                   movel     *blanks       wrkEml          253
009100140225     C                   movel     *blanks       wrkMsg         5000
009200140225     C                   movel     *blanks       wrkOgg           44
009300140225     C* Valorizzo i campi della e-m@ail - indirizzo
009400140225     C                   eval      wrkEml = CED_BART
009500140225     C                   eval      wrkOgg ='Problemi DOWNLD HAGER EDI !!'
009600140225     C                   EVAL      wrkMSG = 'ATTENZIONE : traduzione EDI -
009700140225     C                             STATI << HAGER >> errato su DOWNLD. -
009800140225     C                             Controllare il Download del Cliente. >> '
009900140225     C                   call(e)   'TRTCT00R2'
010000140225     C                   parm                    wrkEml
010100140225     C                   parm                    wrkOgg
010200140225     C                   parm                    wrkMsg
010300140225     C*
010400140225     C                   ENDSR
010500140225      * ?------------------------------------------------------------------ */
