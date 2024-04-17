# source("g:\\R\\popEye\\R\\popEye.R", encoding = "UTF-8")
setwd("./R")
files.sources <- list.files()
sapply(files.sources, source)
setwd("..")

# Run main
maindir <- "t_example_data"
stimfile <- read.table(paste(maindir, "DM_D_13_paragraphs.csv", sep = "/"),
  header = T, sep = ",", fileEncoding = "UTF-8", quote = "\""
)
stimfile <- stimfile[is.na(stimfile$cond) == F, ]
stimfile$text <- gsub(" \\\\n", "\\\\n ", stimfile$text)

exp <- popEye(
  # Provide folder were the Experiment Builder experiment is located:
  datpath = paste(maindir, "DM_D_13_res/", sep = "/"),
  tracker.software = "ET",
  # Specify the type of experiment (sentence, target, boundary-change, text etc.):
  type = "text",

  # Specify which messages have been used to indicate the begin and the end of the trial:
  message.start = "SYNCTIME",
  message.stop = "ENDBUTTON",

  # Specify the variable that indicates the trial number in the DataSource object
  variable.id = "trial",
  variable.cond = "cond",

  # Specify the name of the stimulus file ("stimfile" here)
  stimulus.file = stimfile,
  # Specify the name of the variable in the stimulus file that corresponds to
  # number of the stimulsus (this has to be the same as variable used in in
  # the DataSource object, "number" here, see above)
  stimulus.id = "item",
  # Specify the name of the variable in the stimulus file that corresponds to
  # name of variable that provides the text of the stimulus ("Paragraph" here)
  stimulus.text = "text",
  stimulus.cond = "cond",

  # Specify the number of pixels of the margins at the left, top, and right
  # this should be be same as in the numbers that you have provided in the
  # multiple line text field in Experiment Builder
  display.marginLeft = 429,
  display.marginTop = 181,
  display.marginRight = 50,
  display.marginBottom = 50,

  # Specify the font type and the font size of your experiment, in addition the
  # line spacing has to be provided (single = 1, double = 2 etc.)
  font.name = "Consolas",
  font.size = 17,
  font.spacing = 1,
  clean.stage1Dist = 1,
  clean.stage2Dur = 80,

  indicator.line = "\n",
  assign.lineMethod = "cluster",

  # Specify the name of the output file. If you want to save it at a different
  # location you can use the "outpath" argument
  outpath = maindir,
  outname = "OutputFile",
  item.trigger = 0, # GUESS
)

trialidx = "item.12"
PlotXY(exp, "DM_D_13", trialidx, sub = T, pdf = NULL, interactive = F)
PlotX(exp, "DM_D_13", trialidx, sub = T, pdf = NULL, interactive = F)

PlotY(exp, "DM_D_13", trialidx, sub = T, pdf = NULL, interactive = F)

PlotVelocity(exp, "DM_D_13", trialidx, sub = T, pdf = NULL, interactive = F)

PlotStimulus(exp, "DM_D_13", trialidx, sub = T, interactive = F)
PlotPreprocessing(exp, "DM_D_13", trialidx,  pdf = NULL, interactive = F)

PlotFixations(exp, "DM_D_13", trialidx,  interactive = F)
PlotSentence(exp, "DM_D_13", trialidx,  interactive = F)
PlotTarget(exp, "DM_D_13", trialidx,  interactive = F)
PlotStimulus(exp, "DM_D_13", trialidx,  interactive = F)
PlotAlign(exp,"DM_D_13",trialidx)