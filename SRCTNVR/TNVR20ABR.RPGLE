000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020703
000300091207      * TNVR20ABR *--------------------------------------------------*
000400091207      *  invece di fare il cpyf MAP DROP x sicurezza esegue scrittura
000500020114      *--------------------------------------------------------------*
000600091207      * Testate Tariffe da copiare
000700091207     fFIFPT00Fexif   e             disk    rename(fifpt000:fifptEX)
000800091207     f                                     prefix(Ex_)
000900020703
001000091207     fFIFPD00Fexif   e             disk    rename(fifpd000:fifpdEX)
001100091207     f                                     prefix(Ex_)
001200091207
001300091210
001400091210     fFIFGT00Fexif   e             disk    rename(fifgt000:fifgtEX)
001500091210     f                                     prefix(Ex_)
001600091210
001700091207      * Tariffe con i 5 decimali
001800091207     fFIFPT00F  o  a e             disk
001900091207     fFIFPD00F  o  a e             disk
002000091207      **
002100091210     fFIFGT00F  o  a e             disk
002200091210      **
002300091207      **  Prima esegue la testata
002400091207     c                   read      fiFPT00fEX
002500091207     c                   dow       not %Eof(fiFPT00fEX)
002600091207
002700091207     c                   movel     EX_FPTATB     FPTATB
002800091207     c                   z-add     EX_FPTPDR     FPTPDR
002900091207     c                   movel     EX_FPTSML     FPTSML
003000091207     c                   movel     EX_FPTTSR     FPTTSR
003100091207     c                   z-add     EX_FPTCTR     FPTCTR
003200091207     c                   z-add     EX_FPTPRG     FPTPRG
003300091207     c                   movel     EX_FPTCTD     FPTCTD
003400091207     c                   movel     EX_FPTTAT     FPTTAT
003500091207     c                   movel     EX_FPTTPG     FPTTPG
003600091207     c                   movel     EX_FPTTSM     FPTTSM
003700091207     c                   movel     EX_FPTTPP     FPTTPP
003800091207     c                   z-add     EX_FPTATA     FPTATA
003900091207     c                   z-add     EX_FPTARL     FPTARL
004000091207     c                   z-add     EX_FPTARF     FPTARF
004100091207     c                   z-add     EX_FPTARO     FPTARO
004200091207     c                   z-add     EX_FPTRPV     FPTRPV
004300091207     c                   movel     EX_FPTFLR     FPTFLR
004400091210     c                   movel     *blank        FPTFTR
004500091210     c                   z-add     *zeros        FPTDTR
004600091207     c                   write     fiFPT000
004700091207
004800091207     c                   read      fiFPT00fEX
004900091207     c                   enddo
005000091207      **
005100091207      **  Poi esegue il dettaglio
005200091207     c                   read      fiFPD00fEX
005300091207     c                   dow       not %Eof(fiFPD00fEX)
005400091207
005500091207     c                   movel     EX_FPDATB     FPDATB
005600091207     c                   z-add     EX_FPDPDR     FPDPDR
005700091207     c                   movel     EX_FPDSML     FPDSML
005800091207     c                   movel     EX_FPDTSR     FPDTSR
005900091207     c                   z-add     EX_FPDCTR     FPDCTR
006000091207     c                   z-add     EX_FPDPRG     FPDPRG
006100091207     c                   movel     EX_FPDCTD     FPDCTD
006200091207     c                   movel     EX_FPDCAP     FPDCAP
006300091207     c                   z-add     EX_FPDSGL     FPDSGL
006400091207     c                   z-add     EX_FPDITR     FPDITR
006500091207     c                   z-add     EX_FPDMIN     FPDMIN
006600091207     c                   z-add     EX_FPDMAX     FPDMAX
006700091207     c                   movel     EX_FPDAIN     FPDAIN
006800091210     c                   movel     *blank        FPDFTR
006900091210     c                   z-add     *zeros        FPDDTR
007000091207     c                   write     fiFPD000
007100091207
007200091207     c                   read      fiFPD00fEX
007300091207     c                   enddo
007400020704
007500091210      **
007600091210      **  Poi esegue la testatona
007700091210     c                   read      fiFGT00fEX
007800091210     c                   dow       not %Eof(fiFGT00fEX)
007900091210
008000091210     c                   movel     EX_FGTATB     FGTATB
008100091210     c                   z-add     EX_FGTPDR     FGTPDR
008200091210     c                   movel     EX_FGTSML     FGTSML
008300091210     c                   movel     EX_FGTTSR     FGTTSR
008400091210     c                   z-add     EX_FGTCTR     FGTCTR
008500091210     c                   z-add     EX_FGTPRG     FGTPRG
008600091210     c                   movel     EX_FGTDIV     FGTDIV
008700091210     c                   z-add     EX_FGTDDT     FGTDDT
008800091210     c                   z-add     EX_FGTDST     FGTDST
008900091210     c                   z-add     EX_FGTDUV     FGTDUV
009000091210     c                   movel     EX_FGTDCV     FGTDCV
009100091210     c                   movel     EX_FGTFTP     FGTFTP
009200091214     c                   z-add     EX_FGTDCN     FGTDCN
009300091214     c                   z-add     EX_FGTDTS     FGTDTS
009400091210     c                   movel     *blank        FgtFTR
009500091210     c                   z-add     *zeros        FgtDTR
009600091210     c                   write     fiFGT000
009700091210
009800091210     c                   read      fiFGT00fEX
009900091210     c                   enddo
010000091210
010100020704     c                   Eval      *InLr = *on
010200020704
010300020704      *---------------------------------------------------------------*
