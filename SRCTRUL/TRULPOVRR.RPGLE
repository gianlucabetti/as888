000100031205      *------------------------------------------------------------------------*
000200080708      * Reperisco dati OVRPRTF per printer file
000300031205      *------------------------------------------------------------------------*
000400160302     /*CMD  crtpf file(QTEMP/QADSPOVR) rcdlen(150)
000500160302     /*END  dltf  file(QTEMP/QADSPOVR)
000600160302     /*END
000700160302      *------------------------------------------------------------------------*
000800071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000900160302     h dftactgrp(*no) actgrp(*caller)
001000031205      *------------------------------------------------------------------------*
001100080708     fQADSPOVR  if   e             Disk    rename(qadspovr:qadsp000)
001200080708     f                                     prefix(C_)
001300080704
001400080707     d Indx            s              3  0
001500080709     d Cmd_ovr         s            100
001600080709     d wtipmda         s                   like(吟lptipmda)
001700080709     d wtipm_a         s                   like(吟lptipm_a)
001800080708     d trulpovrds    e ds
001900080708
002000071122      *------------------------------------------------------------------------*
002100080707     c     *entry        plist
002200080708     c                   parm                    trulpovrds
002300071116      /free
002400080709       wtipmda=吟lptipmda   ;
002500080709       wtipm_a=吟lptipm_a   ;
002600080709       clear   trulpovrds   ;
002700080709       clear   cmd_ovr      ;
002800080709
002900080708       setll  *start    QADSPOVR   ;
003000080708       read             QADSPOVR   ;
003100080708
003200080708       dow   not  %eof(QADSPOVR)   ;
003300080708
003400080708       Indx=%scan('OUTQ':c_qadspovr:40) ;
003500080708         if Indx>0    ;
003600080708         助LPOOUTQ =%SUBST(c_QADSPOVR:52:10)  ;
003700080709         // Preparo la stringa della ovrprtf
003800080709         cmd_ovr='OUTQ(' + 助LPOOUTQ +')'        ;
003900080708         endif        ;
004000080708
004100080708       Indx=%scan('FORMTYPE':c_qadspovr:40) ;
004200080708         if Indx>0    ;
004300080708         助LPOFTYPE=%SUBST(C_QADSPOVR:52:10)  ;
004400080708
004500080709         if   wtipmda<>' '   ;
004600080709         Indx=%scan(wtipmda:助LPOFTYPE) ;
004700080708           IF Indx>0    ;
004800080709            %subst(吟lpoftype:Indx:2)=wtipm_a ;
004900080708           endif        ;
005000080709           endif        ;
005100080709         cmd_ovr=%trim(cmd_ovr) + ' FORMTYPE('+ 助LPOFTYPE +')' ;
005200080708         endif        ;
005300080708
005400080708       Indx=%scan('USRDTA':c_qadspovr:40)    ;
005500080708         if Indx>0    ;
005600080708         助LPOUDTA =%SUBST(c_QADSPOVR:52:10)  ;
005700080708
005800080709         if   wtipmda<>' '   ;
005900080709         Indx=%scan(wtipmda:助LPOudta ) ;
006000080708           IF Indx>0    ;
006100080709            %subst(吟lpoudta :Indx:2)=wtipm_a ;
006200080708           endif        ;
006300080709           endif        ;
006400080715         cmd_ovr=%trim(cmd_ovr) + ' USRDTA('''+ 助LPOUDTA + ''')'  ;
006500080708         endif        ;
006600090605
006700090605       Indx = %scan('SAVE' : c_qadspovr : 40);
006800090605         if Indx > *zero;
006900090605           助LPOsave = %subst(c_QADSPOVR : 52 : 5);
007000090610           cmd_ovr = %trim(cmd_ovr) + ' SAVE(' + %trimr(助LPOsave) +')';
007100090605         endif;
007200080708
007300080708       Indx=%scan('SHARE':c_qadspovr:40)    ;
007400080708         if Indx>0    ;
007500080708         助LPOSHARE=%SUBST(c_QADSPOVR:52:10)  ;
007600080709         cmd_ovr=%trim(cmd_ovr) + ' SHARE(' +助LPOSHARE + ')'  ;
007700080708         endif        ;
007800080708
007900080708       read             QADSPOVR   ;
008000080708       enddo                       ;
008100080708
008200080708       // Se sono tutti vuoti vuol dire che non ho fatto nessa OVRDBF e
008300080708       //   restituisco errore
008400080708
008500080709        吟lptipmda=wtipmda   ;
008600080709        吟lptipm_a=wtipm_a   ;
008700080709
008800080708       if 吟lposhare=*blanks   and
008900080708        吟lpooutq =*blanks   and
009000080708        吟lpoftype=*blanks   and
009100080708        吟lpoudta =*blanks       ;
009200080708        吟lpoerr='E'             ;
009300080709        else    ;
009400080709        吟lpocmo=cmd_ovr     ;
009500080708        endif                    ;
009600080708
009700080709       *inlr = *on   ;
009800080708
009900071116      /end-free
