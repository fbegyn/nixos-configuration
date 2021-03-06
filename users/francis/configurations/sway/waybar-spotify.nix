{ pkgs }:

pkgs.writeScriptBin "waybar-spotify.sh" ''
#! /usr/bin/env sh

class=$(${pkgs.playerctl}/bin/playerctl metadata --player=spotify --format '{{lc(status)}}')
icon="🎘"

if [[ $class == "playing" ]]; then
  info=$(${pkgs.playerctl}/bin/playerctl metadata --player=spotify --format '{{artist}} - {{title}}')
  if [[ $\{#info} > 40 ]]; then
    info=$(echo $info | cut -c1-40)\"...\"
  fi
  text=$info" "$icon
elif [[ $class == \"paused\" ]]; then
  text=$icon
elif [[ $class == \"stopped\" ]]; then
  text=""
fi

echo -e "{\"text\":\""$text"\", \"class\":\""$class"\"}"
''
