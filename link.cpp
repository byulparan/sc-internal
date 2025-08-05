#include <ableton/Link.hpp>


extern "C" {
  void LinkEnable(float bpm);
  void LinkDisable();
  float LinkGetTempo();
  void LinkSetTempo(float bpm);
  double LinkGetTime();
  double LinkGetBeat();
  double LinkGetBeatToTime(double beat);
  double LinkGetTimeToBeat(double time);
}

static ableton::Link *gLink = nullptr;


void LinkEnable(float bpm) {
  if (!gLink) {
    gLink = new ableton::Link(bpm);
  }
  gLink->enable(true);
}

void LinkDisable() {
  if(gLink) {
    gLink->enable(false);
  }
}

float LinkGetTempo() {
  float output = 0.0;
  if(gLink) {
    auto timeline = gLink->captureAppSessionState();
    output = timeline.tempo();
  }
  return output;
}


void LinkSetTempo(float bpm) {
  if (gLink) {
    auto timeline = gLink->captureAudioSessionState();
    timeline.setTempo(bpm, gLink->clock().micros());
    gLink->commitAudioSessionState(timeline);
  }
}


double LinkGetTime() {
  double output = 0.0;
  if (gLink) {
    output = gLink->clock().micros().count() * 1e-6;
  }
  return output;
}


double LinkGetBeat() {
  double output = 0.0;
  if (gLink) {
    const auto time = gLink->clock().micros();
    auto timeline = gLink->captureAppSessionState();
    const auto beats = timeline.beatAtTime(time, 4);
    output =  beats;
  }
  return output;
}



double LinkGetBeatToTime(double beat) {
  double output = 0.0;
  if (gLink) {
    auto timeline = gLink->captureAppSessionState();
    auto time = timeline.timeAtBeat(beat, 4);
    output = time.count() * 1e-6;
  }
  return output;
}

double LinkGetTimeToBeat(double time) {
  double output = 0.0;
  if (gLink) {
    auto timeline = gLink->captureAppSessionState();
    std::chrono::microseconds us =
      std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::duration<double>(time));
    output = timeline.beatAtTime(us, 4);
  }
  return output;
}






